{-# LANGUAGE PatternSynonyms #-}

module GenProc.Env where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Util

import Data.Set (Set)
import qualified Data.Set as Set

import GenProc.BuildExp

import Prelude hiding (exp) -- Removes name shadowing warnings.

data Env = PairEnv Env Env | VarEnv (Name ()) | Empty
    deriving (Show)
-- An environment with some number of args alongside it (may be zero).
-- This is structured in code as (env, args) in general. Three cases:
-- 1. WithArgs env 0 - we just have env as there are no args.
-- 2. WithArgs env 1 - we have (env, arg1).
-- 3. WithArgs env n where n>1 - we have (env, (arg1, restofargs))
data WithArgs = WithArgs Env Int

-- Given a WithArgs and an Exp to run on only the Env part, return an Exp that
-- reduces the WithArgs down to the right Env and runs the input Env.
runWithoutArgs :: WithArgs -> Exp () -> Exp ()
runWithoutArgs (WithArgs _ 0) exp = exp
runWithoutArgs (WithArgs _ _) exp = callExp "first" exp

-- Given an Exp, look at all of the variables within it and create a new variable that
-- is not used within that Exp. This is used to create a name which cannot accidentally
-- shadow one from a parent scope.
summonUnusedName :: Exp () -> Set (Name ()) -> String -> String
summonUnusedName exp bound prefix = let
    free = freeVars exp
    disallowedVars = idents $ Set.union bound free
    in if Set.member prefix disallowedVars then tryName prefix 0 disallowedVars else prefix
    where
        tryName :: String -> Int -> Set String -> String
        tryName nam num disallowed
            | Set.member (nam ++ show num) disallowed = tryName nam (num+1) disallowed
            | otherwise = nam ++ show num

idents :: Set (Name ()) -> Set String
idents = Set.map toStringOnly
    where
        toStringOnly :: Name () -> String
        toStringOnly (Ident () st) = st
        toStringOnly (Symbol () st) = st

envNames :: Env -> Set (Name ())
envNames (VarEnv nm) = Set.singleton nm
envNames (PairEnv l r) = Set.union (envNames l) (envNames r)
envNames Empty = Set.empty

patNames :: Pat () -> Set (Name ())
patNames = envNames . snd . patToExpEnv

patWithArgs :: WithArgs -> Pat () -> String -> Pat ()
patWithArgs (WithArgs _ 0) pat _ = pat
patWithArgs (WithArgs _ _) pat nm = PairPat pat (PVar () $ Ident () nm)

expWithArgs :: WithArgs -> Exp () -> String -> Exp ()
expWithArgs (WithArgs _ 0) exp _ = exp
expWithArgs (WithArgs _ _) exp nm = PairExp exp (Var () $ UnQual () $ Ident () nm)

-- ***
-- Merge environments.
-- ***

-- Given two Env, merge them to remove duplicate keys, preferring entries in the first Env.
-- This returns the new Env and a THExp from (env1, env2) to that new Env.
-- INVARIANT: Neither individual Env contains duplicate keys.
mergeEnv :: Env -> Env -> (Exp (), Env)
mergeEnv env1 env2 = case filterEnv env2 keepFromEnv2 of
        Just (env', rout) -> let exp = routingFnAsExp rout in
            (callExp "second" exp, PairEnv env1 env')
        Nothing -> (varExp "dropR", env1)
    where
        keepFromEnv2 = Set.difference (envNames env2) (envNames env1)

-- mergeEnv, but preferring entries in the second env.
mergeEnvRev :: Env -> Env -> (Exp (), Env)
mergeEnvRev env1 env2 = case filterEnv env1 keepFromEnv1 of
        Just (env', rout) -> let exp = routingFnAsExp rout in
            (callExp "first" exp, PairEnv env' env2)
        Nothing -> (varExp "dropL", env1)
    where
        keepFromEnv1 = Set.difference (envNames env1) (envNames env2)

-- ***
-- Convert between different forms to and from Env.
-- ***

pattern PairPat :: Pat () -> Pat () -> Pat ()
pattern PairPat l r = PApp () (Qual () (ModuleName () "GenProc.GeneralisedArrow") (Ident () "Pair")) [l, r]

pattern OnePat :: Pat () -> Pat ()
pattern OnePat e = PApp () (Qual () (ModuleName () "GenProc.GeneralisedArrow") (Ident () "One")) [e]

pattern PairExp :: Exp () -> Exp () -> Exp ()
pattern PairExp l r = App () (App () (Con () (Qual () (ModuleName () "GenProc.GeneralisedArrow") (Ident () "Pair"))) l) r

pattern OneExp :: Exp () -> Exp ()
pattern OneExp e = App () (Con () (Qual () (ModuleName () "GenProc.GeneralisedArrow") (Ident () "One"))) e

-- Pattern matching by transforming an output of the form of the pattern into
-- an env and the exp required to perform that transformation.
-- Used for the output of -< and <-, to make it fit the required structure.
-- This says "if we have a value fitting this pattern, we can apply exp to it to get env".
patToExpEnv :: Pat () -> (Exp (), Env)
patToExpEnv (PParen () pat) = let
    (exp, env) = patToExpEnv pat
    in (Paren () exp, env)
-- If we have p <- f -< x, then we're adding One p to the env.
-- Since f outputs One p, we just take that One p.
patToExpEnv (PVar () nm) = (idExp, VarEnv nm)
-- If we have {p, q} <- f -< x, then the output of f must return Pair.
-- Therefore we transform each of p and q in turn, and apply them to
-- the components of that Pair.
patToExpEnv (PairPat l r) = let
    (le, ln) = patToExpEnv l
    (re, rn) = patToExpEnv r
    in (parallelExp le re, PairEnv ln rn)
-- For other patterns, we need to use an arr to map that pattern to an env.
-- envToExp will call One/Pair as is needed, thus building a Val that correctly
-- corresponds to the required env.
patToExpEnv pat = let
    Vars boundV _ = allVars pat
    env = varsToEnv $ Set.toList boundV
    exp = envToExp env
    in (arrExp $ Lambda () [pat] exp, env)

-- Takes a list of expressions and pairs them up
pairExpEnvs :: [(Exp (), Env)] -> (Exp (), Env)
pairExpEnvs [] = undefined
pairExpEnvs [e] = e
pairExpEnvs ((exp, env) : es) = let
        (exps, envs) = pairExpEnvs es
    in (PairExp (OneExp exp) exps, PairEnv env envs)

envToPat :: Env -> Pat ()
envToPat (VarEnv nm) = OnePat $ PVar () nm
envToPat (PairEnv l r) = PairPat (envToPat l) (envToPat r)
envToPat Empty = PWildCard ()

envToExp :: Env -> Exp ()
envToExp (VarEnv nm) = OneExp $ Var () (UnQual () nm)
envToExp (PairEnv l r) = PairExp (envToExp l) (envToExp r)
envToExp Empty = OneExp $ Con () (Special () (UnitCon ())) 

varsToEnv :: [Name ()] -> Env
varsToEnv [] = Empty
varsToEnv [nam] = VarEnv nam
varsToEnv [naml, namr] = PairEnv (VarEnv naml) (VarEnv namr)
varsToEnv many = let (l, r) = bisect many in PairEnv (varsToEnv l) (varsToEnv r)
    where
        bisect :: [a] -> ([a], [a])
        bisect xs = splitAt (length xs `div` 2) xs

-- ***
-- Express routing functions, for routing data without having to build Exps.
-- ***

data RoutingFn = DropL | DropR | Dup | RoutId
    | RoutingPair RoutingFn RoutingFn | RoutingComp RoutingFn RoutingFn
    | EmptyRout

filterToEnvExp :: Env -> Set (Name ()) -> (Env, Exp ())
filterToEnvExp env nms = case filterEnv env nms of
    Just (env', rout) -> (env', routingFnAsExp rout)
    Nothing -> (Empty, routingFnAsExp EmptyRout)

filterEnv :: Env -> Set (Name ()) -> Maybe (Env, RoutingFn)
filterEnv env st = filterEnv' env
    where
        filterEnv' :: Env -> Maybe (Env, RoutingFn)
        filterEnv' e@(VarEnv nm) = if Set.member nm st then Just (e, RoutId) else Nothing
        filterEnv' (PairEnv l r) = case (filterEnv' l, filterEnv' r) of
            (Just (el, rl), Just (er, rr)) -> Just (PairEnv el er, RoutingPair rl rr)
            (Just (el, rl), Nothing) -> Just (el, RoutingComp DropR rl)
            (Nothing, Just (er, rr)) -> Just (er, RoutingComp DropL rr)
            (Nothing, Nothing) -> Nothing
        filterEnv' Empty = Nothing

routingFnAsExp :: RoutingFn -> Exp ()
routingFnAsExp DropL = varExp "dropL"
routingFnAsExp DropR = varExp "dropR"
routingFnAsExp Dup = varExp "dup"
routingFnAsExp RoutId = idExp
routingFnAsExp (RoutingPair l r) = let
    le = routingFnAsExp l
    re = routingFnAsExp r
    in parallelExp le re
routingFnAsExp (RoutingComp l r) = let
    le = routingFnAsExp l
    re = routingFnAsExp r
    in composeExp le re
routingFnAsExp EmptyRout = callExp "constant" (OneExp $ Con () (Special () (UnitCon ())))

-- Route from one env to another using drop/dup
routeEnvToEnv :: Env -> Env -> RoutingFn
routeEnvToEnv env (PairEnv l r) = let
    lr = routeEnvToEnv env l
    rr = routeEnvToEnv env r
    in RoutingComp Dup (RoutingPair lr rr)
routeEnvToEnv env (VarEnv nam) = case reduceTo env nam of
        Just rf -> rf
        Nothing -> error "Cannot route between envs provided."
    where
        reduceTo :: Env -> Name () -> Maybe RoutingFn
        reduceTo (VarEnv nm) nm' = if nm == nm' then Just RoutId else Nothing
        reduceTo (PairEnv l r) nm = case reduceTo l nm of
            Just rf -> Just $ RoutingComp DropR rf
            Nothing -> RoutingComp DropL <$> reduceTo r nm
        reduceTo Empty _ = Nothing
routeEnvToEnv _ Empty = EmptyRout

-- Given an environment and a set of variables that we will use, remove all others.
removeUnusedBindings :: WithArgs -> Set (Name ()) -> (WithArgs, Exp ())
removeUnusedBindings wa@(WithArgs env n) st = case filterEnv env st of
    Just (env', rout) -> (WithArgs env' n, runWithoutArgs wa $ routingFnAsExp rout)
    Nothing -> (WithArgs Empty n, runWithoutArgs wa $ routingFnAsExp EmptyRout)