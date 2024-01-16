module GenProc.TransformProc (transformProc) where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Util

import GenProc.BuildExp
import GenProc.Env

import qualified Data.Set as Set

import Prelude hiding (exp) -- Removes name shadowing warnings.
import qualified Prelude as P

-- Transform Proc pat exp into a chain of commands.
-- This populates the env with the contents of pat, and uses it to compile exp as a command.
transformProc :: Pat () -> Exp () -> Exp ()
transformProc pat exp = let
    (router, env) = patToExpEnv pat
    in composeExp router (transformCommand exp $ WithArgs env 0)

-- Takes an exp to its arrow equivalent, given an environment of named variables we've seen already.
-- This environment may have a stack of args associated with it (stored in WithArgs as the number of args).
-- NOTE: No env is returned as the env will always be an unnamed value on the output of the arrow program.
-- NOTE: In a few places, we remove unused bindings via expToRouting or removeUnusedBindings.
-- This makes sure that dependency information is preserved - the desugaring makes a lot of use of arr, and
-- if we naively do e.g. arr (\(entire env) -> exp), then we are assuming that exp relies on the entire env
-- as arr is a black box. This is bad, as a fair amount of our dependency analysis work in rearrange will break!
-- Therefore, we first filter out bindings which are not used within exp before they reach the arr using drop,
-- thus meaning that it no longer looks like they are needed to compute env.
-- INVARIANT: There are no repeats on the input Env.
transformCommand :: Exp () -> WithArgs -> Exp ()
transformCommand (Paren () exp) wa = Paren () $ transformCommand exp wa

-- For f -< inp, we need to map the environment to inp, then apply f.
-- In do-notation, this will either be discarded (f -< x), given a name (a <- f -< x) or will be the last statement and thus the return value.
-- This runs inp without the args since it cannot consume them, but otherwise passes them
-- through to f. (Turns out you can have `((\y -> f) -< x) arg`.)
transformCommand (LeftArrApp () f inp) wa@(WithArgs env _) =
    composeExp (runWithoutArgs wa $ expToRouting inp env) f
transformCommand (RightArrApp () inp f) wa = transformCommand (LeftArrApp () f inp) wa

-- For f -<< e, we need to also find freeVars of f as well as e. We therefore run expToRouting on both arguments rather than just e.
-- Again, no args allowed.
transformCommand (LeftArrHighApp () f inp) wa@(WithArgs env n) =
    -- In the n=0 case we get:
    -- dup >>> (expToRouting f *** expToRouting inp) >>> app
    -- where expToRouting evaluates its input using the env.
    -- In the other case we get:
    -- first (dup >>> (expToRouting f *** expToRouting inp)) >>> assoc >>> app
    -- since f and inp are evauated without access to the args?
    -- The rules are a little confusing here.
    let
        evaluateInps = runWithoutArgs wa $ composeExp (varExp "dup") $
            parallelExp (expToRouting f env) (expToRouting inp env)
        mergeInArgs = case n of
            0 -> idExp
            _ -> varExp "assoc"
        in composeExp evaluateInps (composeExp mergeInArgs (varExp "app"))
transformCommand (RightArrHighApp () inp f) env = transformCommand (LeftArrHighApp () f inp) env

-- Transform the given do.
-- Since transformDo always returns (result, env), drop the env.
-- There cannot be any args as you can't consume them like this - the closest you get is
-- (do
--   \y -> ...
--   ...) arg
-- which doesn't make sense, you don't replace y with arg.
transformCommand (Do () stmts) (WithArgs env 0) = let
    (thexp, _) = transformDo stmts env
    in composeExp thexp $ varExp "dropR"

-- Transform if into arr (\env -> if cond then Left env else Right env) >>> (l ||| r).
-- This propagates args to l and r.
-- Therefore, we evaluate cond without args (using first for n>0) and then evaluate l/r with args.
transformCommand (If () cond l r) wa = let
    -- Remove variables which aren't used by this if block.
    used = Set.unions [freeVars l, freeVars r, freeVars cond]
    (wa'@(WithArgs env n), removeUnused) = removeUnusedBindings wa used
    -- Identify which variables are used by l and r, building (tuple of free vars, env) for each.
    lenv = varsToEnv $ Set.toList $ Set.intersection (freeVars l) (envNames env)
    lexp = envToExp lenv
    renv = varsToEnv $ Set.toList $ Set.intersection (freeVars r) (envNames env)
    rexp = envToExp renv
    -- Make arr (\(env, nam) -> if cond then Left (free l, nam) else Right (free r, nam))
    nam = summonUnusedName cond (envNames env) "stack"
    pat' = patWithArgs wa' (envToPat env) nam
    lout = expWithArgs wa' lexp nam
    rout = expWithArgs wa' rexp nam
    condArr = Lambda () [pat']
        (If () cond (conExp "Choice1" lout) (conExp "Choice2" rout))
    -- Compile l and r with their reduced envs
    lth = transformCommand l (WithArgs lenv n)
    rth = transformCommand r (WithArgs renv n)
    -- runWithoutArgs (arr condArr) >>> mergeInArgs >>> (lth ||| rth)
    in composeExp (composeExp removeUnused $ arrExp condArr) (symbolExp "|||" lth rth)

-- Take the case we've already been provided, of the form
-- Case () exp [Alt () pat1 rhs1, Alt () pat2 rhs2, ...]
-- and transform it into
-- arr (\(env, stack) -> Case () exp [Alt () pat1 (Left free1), Alt () pat2 (Right (Left free2)), ...])
--     >>> (rhs1 ||| (rhs2 ||| ...))
transformCommand (Case () exp alts) wa = let
    -- Remove variables which aren't used by this case block or any of its cases.
    used = Set.union (freeVars exp) $ Set.unions $ map freeVars alts
    (wa'@(WithArgs env n), removeUnused) = removeUnusedBindings wa used
    -- Generate a list of constructors [Left . Right, Left . Left] and so on for use with the case desugaring.
    altsCount = length alts
    constructors = makeEitherConstructors altsCount
    stackName = summonUnusedName exp (Set.union (freeVars alts) (envNames env)) "stack"
    -- Use those constructors to transform each Alt of pat -> exp into the new alt pat -> constr env,
    -- returning the output of each alt separately (exps).
    (alts', exps) = unzip $ zipWith (buildArr wa' stackName) alts constructors
    -- Join those outputs together using |||.
    choices = buildChoiceTree exps
    -- Build an arr using alts' which returns the env within some constructor (Left . Right etc).
    pwa = patWithArgs wa' (envToPat env) stackName
    newCase = arrExp $ Lambda () [pwa] $ Case () exp alts'
    in composeExp (composeExp removeUnused newCase) choices
    where
        -- Given an alt from the original case pat -> exp, construct a new alt pat -> constr env.
        buildArr :: WithArgs -> String -> Alt () -> (Exp () -> Exp ()) -> (Alt (), Exp ())
        buildArr wa@(WithArgs env n) stackName (Alt () pat (UnGuardedRhs () rhs) _) constr = let
            -- Output of this case should be all of env and pat that are used in rhs.
            env' = varsToEnv $ Set.toList $
                Set.intersection (freeVars rhs) (Set.union (envNames env) (patNames pat))
            output = envToExp env'
            rhs' = transformCommand rhs (WithArgs env' n)
            outputWithArgs = expWithArgs wa output stackName
            in (Alt () pat (UnGuardedRhs () (constr outputWithArgs)) Nothing, rhs')
        buildArr _ _ _ _ = error "Failed to build case."

        -- Given the number of cases, makes that many unique constructors by splitting them between left
        -- and right. Invariant is n > 1.
        makeEitherConstructors :: Int -> [Exp () -> Exp ()]
        makeEitherConstructors 1 = [P.id]
        makeEitherConstructors conCount =
            let (l, r) = halve conCount in
            map (\constr v -> conExp "Choice1" (constr v)) (makeEitherConstructors l) ++
            map (\constr v -> conExp "Choice2" (constr v)) (makeEitherConstructors r)
            where
                halve :: Int -> (Int, Int)
                halve n = let (q, r) = quotRem n 2 in (q, q+r)
        
        -- Consume Exp from the list using the same splitting mechanism as makeEitherConstructors,
        -- joining them with |||.
        buildChoiceTree :: [Exp ()] -> Exp ()
        buildChoiceTree [e] = e
        buildChoiceTree exps = let
            (lsub, rsub) = splitAt (quot (length exps) 2) exps
            lhs = buildChoiceTree lsub
            rhs = buildChoiceTree rsub
            in symbolExp "|||" lhs rhs

-- Need to construct:
-- arr (\(env, stack) -> let binds in (freeVars exp)) >>> transformCommand exp env'
transformCommand (Let () binds exp) wa = let
    -- Remove variables which are unused by either the let bindings or the exp itself.
    Vars boundVars freeInBinds = allVars binds
    used = Set.union freeInBinds (freeVars exp)
    (wa'@(WithArgs env n), removeUnused) = removeUnusedBindings wa used
    -- Build arr (\(env, stack) -> let ... in (used by exp, stack)).
    envlet = varsToEnv $ Set.toList $
        Set.intersection (freeVars exp) $ Set.union boundVars (envNames env)
    letexp = envToExp envlet
    stackName = summonUnusedName exp (Set.union freeInBinds (envNames env)) "stack"
    pwa = patWithArgs wa' (envToPat env) stackName
    letarr = arrExp $ Lambda () [pwa] $ Let () binds letexp
    -- Run the output exp with the newly modified environment, containing the binds
    -- and any parts of env used in exp.
    exp' = transformCommand exp (WithArgs envlet n)
    in composeExp (composeExp removeUnused letarr) exp'
    
-- Adds the leftmost argument to the environment as the given name.
transformCommand (Lambda () [PVar () nm] exp) (WithArgs env n) =
    case n of
        0 -> error "Cannot have a lambda without any arguments to consume."
        -- The structure has a single arg on it, so we just modify the env.
        -- We go from (env, arg1) to env' with that arg1 named nm.
        1 -> transformCommand exp (WithArgs (PairEnv env (VarEnv nm)) 0)
        -- The structure has multiple args on it, so we're going from
        -- (env, (arg1, rest)) to ((env, arg1), rest) and name arg1.
        -- Hey that's unassoc.
        _ -> composeExp (varExp "unassoc") $
            transformCommand exp (WithArgs (PairEnv env (VarEnv nm)) (n-1))
transformCommand (Lambda () (p : ps) exp) wa =
    transformCommand (Lambda () [p] $ Lambda () ps exp) wa
transformCommand (App () c e) wa@(WithArgs env n) = let
    -- With n=0 this is
    -- dup >>> second e
    -- which gives us (env, arg1)
    -- With n>0 this is
    -- first (dup >>> second e)
    -- which gives us ((env, arg1), rest)
    evaluateE = runWithoutArgs wa (composeExp (varExp "dup") (callExp "second" e))
    evaluateC = transformCommand c (WithArgs env (n+1))
    in case n of
        -- There were no arguments previously, so the structure is already correct:
        0 -> composeExp evaluateE evaluateC
        -- Otherwise we need to assoc to fix the structure:
        _ -> composeExp evaluateE (composeExp (varExp "assoc") evaluateC)

transformCommand _ _ = undefined

-- Transform a Qualifier () exp using env, which does not touch the environment.
-- THExp has the form env -> (output of exp, env).
transformQualifier :: Exp () -> Env -> Exp ()
transformQualifier exp env =
    let transformed = transformCommand exp (WithArgs env 0)
    in composeExp (varExp "dup") (callExp "first" transformed)

-- Transform a Qualifier () exp like above, except drop the output of exp.
-- THExp has the form env -> env, but with the effects of exp.
transformQualifierDrop :: Exp () -> Env -> Exp ()
transformQualifierDrop exp env = composeExp (transformQualifier exp env) (varExp "dropL")

-- Transform a Generator () pat exp using env, which runs exp and assigns its result to pat.
-- This updates the environment, which is merged with the previous one and returned as env'.
-- ThExp has the form env -> env'.
transformGenerator :: Pat () -> Exp () -> Env -> (Exp (), Env)
transformGenerator pat exp env =
    let thexp = transformCommand exp (WithArgs env 0)
        (routing, patenv) = patToExpEnv pat
        (merger, env') = mergeEnv patenv env
    -- dup >>> first (thexp >>> routing) >>> merger
    in (composeExp (varExp "dup") (composeExp (callExp "first" (composeExp thexp routing)) merger), env')

-- Transform a Let () (BDecls () decls) using env.
-- let bs; translates to arr (\free vars in bs -> (bound vars in bs)) >>> ...
-- So we transform let bs into let bs in (bound vars in bs), and let expToRouting do all the work.
-- We then mergeEnv to remove any duplicate definitions from the env.
-- ThExp has the form env -> env'.
transformLet :: [Decl ()] -> Env -> (Exp (), Env)
transformLet decls env =
    let Vars boundVars _ = allVars decls
        envlet = varsToEnv $ Set.toList boundVars
        exp = envToExp envlet
        toTransform = Let () (BDecls () (map transformInnerProcs decls)) exp
        -- NOTE This is an inlined modified version of expToRouting,
        -- which cuts out a few cases which can't be possible and
        -- doesn't encase its final result in One.
        -- This is necessary as toTransform has its final result
        -- already encased in One.
        filtered = case filterEnv env (freeVars toTransform) of
            Just (env', rout) ->
                composeExp (routingFnAsExp rout) $
                    arrExp $ Lambda () [envToPat env'] toTransform
            Nothing -> callExp "constant" toTransform
        (merger, env') = mergeEnv envlet env
    -- dup >>> first thexp >>> merger
    in (composeExp (varExp "dup") (composeExp (callExp "first" filtered) merger), env')

-- TODO This is very fragile.
transformInnerProcs :: Decl () -> Decl ()
-- A `let x = e` is a PatBind () (Ident () "x") (UnGuardedRhs () e)
transformInnerProcs (PatBind () pat (UnGuardedRhs () (Proc () pat' exp)) Nothing) =
    PatBind () pat (UnGuardedRhs () (transformProc pat' exp)) Nothing
transformInnerProcs d = d

-- Transform a RecStmt () stmts using env, which creates a loop which loops back variables which are defined
-- after their use within the rec.
-- The THExp is of the form env -> env', since rec discards its value (if it has one).
transformRecStmt :: [Stmt ()] -> Env -> (Exp (), Env)
transformRecStmt stmts env = let
    Vars boundV freeV = allVars stmts
    -- These are the variables provided on the loop backedge.
    useBeforeBind = Set.intersection boundV freeV
    -- Merge this looped env with the existing one
    loopedEnv = varsToEnv (Set.toList useBeforeBind)
    (merger, env') = mergeEnvRev env loopedEnv
    (thexp, env'') = transformRec stmts env'
    -- This should make a TExp of form env'' -> (env'', loopedEnv).
    -- This is a different process to filterEnv as it is key that we do not use arr here.
    -- NOTE We guarantee no duplicates, so can use swaps etc to avoid dup/drop chains?
    reduceToLoopEnv = routingFnAsExp $ routeEnvToEnv env'' loopedEnv
    -- loop (marger >>> thexp >>> dup >>> second reduceToLoopEnv)
    in (loopExp (composeExp merger (composeExp thexp (composeExp (varExp "dup") (callExp "second" reduceToLoopEnv)))), env'')

-- Transform do-notation within a proc into a long chain of arrow operations.
-- The THExp will be an arrow of the form env -> (return of stmts, env') where env, env' are the Env in/output.
transformDo :: [Stmt ()] -> Env -> (Exp (), Env)
-- A final exp, which is the returned value.
transformDo [Qualifier () exp] env = (transformQualifier exp env, env)
transformDo [_] _ = error "A do block must end with a single qualifier (that is, not a generator a <- b, nor a rec or let)."
-- In this case, we are discarding the result of an action, but still need to do it.
transformDo (Qualifier () exp : xs) env =
    let thexp = transformQualifierDrop exp env
        (thexps, env') = transformDo xs env
    in (composeExp thexp thexps, env')
-- pat <- exp, so we need to run exp and put it in the env as pat.
-- We merge the environments to remove any duplications.
transformDo (Generator () pat exp : xs) env =
    let (thexp, env') = transformGenerator pat exp env
        (thexps, env'') = transformDo xs env'
    in (composeExp thexp thexps, env'')
transformDo (LetStmt () (BDecls () decls) : xs) env = let
    (thexp, env') = transformLet decls env
    (thexps, env'') = transformDo xs env'
    in (composeExp thexp thexps, env'')
transformDo (RecStmt () stmts : xs) env = let
    (thexp, env') = transformRecStmt stmts env
    (thexps, env'') = transformDo xs env'
    in (composeExp thexp thexps, env'')
transformDo _ _ = error "unimplemented"

-- Transform rec notation into a long chain of arrow operations.
-- The THExp will be an arrow of the form env -> env' where env, env' are the Env in/output.
transformRec :: [Stmt ()] -> Env -> (Exp (), Env)
-- This differs from transformDo in its base cases.
transformRec [Qualifier () exp] env = (transformQualifierDrop exp env, env)
transformRec [Generator () pat exp] env = transformGenerator pat exp env
transformRec [LetStmt () (BDecls () decls)] env = transformLet decls env
transformRec [RecStmt () stmts] env = transformRecStmt stmts env
transformRec (Qualifier () exp : xs) env =
    let thexp = transformQualifierDrop exp env
        (thexps, env') = transformRec xs env
    in (composeExp thexp thexps, env')
transformRec (Generator () pat exp : xs) env =
    let (thexp, env') = transformGenerator pat exp env
        (thexps, env'') = transformRec xs env'
    in (composeExp thexp thexps, env'')
transformRec (LetStmt () (BDecls () decls) : xs) env = let
    (thexp, env') = transformLet decls env
    (thexps, env'') = transformRec xs env'
    in (composeExp thexp thexps, env'')
transformRec (RecStmt () stmts : xs) env = let
    (thexp, env') = transformRecStmt stmts env
    (thexps, env'') = transformRec xs env'
    in (composeExp thexp thexps, env'')
transformRec _ _ = error "unimplemented"

-- Extract the free variables in the expr, and then create a function routing from the env to those free variables.
-- Finally, combine those variables into the required expr via a call to arr.
-- INVARIANT: There are no repeated definitions within the Env.
expToRouting :: Exp () -> Env -> Exp ()
-- Special case to deal with tuples, which shouldn't use arr as this breaks dependency information, and
-- tuples refer to separate variables so have special dependency information.
expToRouting (Paren () e) env = Paren () $ expToRouting e env
expToRouting (PairExp l r) env = let
    makeL = expToRouting l env
    makeR = expToRouting r env
    in composeExp (varExp "dup") (parallelExp makeL makeR)
expToRouting exp env = case filterEnv env (freeVars exp) of
        Just (env', rout) ->
        -- If we use anything from the Env, we have a routing function rout from env to env'.
            -- Note that if exp uses anything outside of the env (e.g. in the code surrounding the proc),
            -- then it'll be found nicely in the arr since all variables outside of the env are in scope in that arr.
            let expFromEnv' = case exp of
                    -- If exp is a single var, then filterEnv will filter the env down to
                    -- One v, and thus we already have what we need.
                    Var () _ -> idExp
                    _ -> arrExp $ Lambda () [envToPat env'] (OneExp exp)
            -- routingFnAsExp rout >>> expFromEnv
            in composeExp (routingFnAsExp rout) expFromEnv'
        -- If we use nothing from Env, then the routing function is `constant exp`.
        Nothing -> callExp "constant" (OneExp exp)
