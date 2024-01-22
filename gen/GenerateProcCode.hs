module GenerateProcCode (generateProcCode, ProcCode(..), Line(..), Var, SingleVar, PairVar) where

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

-- Proc code consists of some number of lines of code and then a numbered Var
data ProcCode = PC [Line] SingleVar
    deriving (Show)
-- Each line is vj <- op -< vs, where j is the line number and vs are the input(s).
-- e.g. Pre v vl vr === vr <- pre v -< vl.
data Line = Pre Double SingleVar SingleVar | Add PairVar SingleVar | Sub PairVar SingleVar
    | Mul PairVar SingleVar | Inc SingleVar SingleVar | RecLine [Line]
    deriving (Show)
-- Ints are used to represent vars, with variable i being called v_i.
type Var = Int
type SingleVar = Var
type PairVar = (Var, Var)
data InputShape = ShapeSV | ShapePV | RecStmt Int [InputShape]

data VarSet = VS {
    unused :: Set Var,
    used :: Set Var,
    repeatsAllowed :: Int }

-- Repeats allowed is simply the number of input slots minus the number of output
-- ones, as we need to use every output variable at least once as an input.
defaultVarSet :: [InputShape] -> VarSet
defaultVarSet inps = VS (Set.singleton 0) Set.empty (varsLeft inps - newVars inps)

-- Given a list of InputShape, give us the number of times we will need an input.
-- Lines with single inputs count as one, pairs count as two and Rec k is counts
-- as k + the number of vars inside.
varsLeft :: [InputShape] -> Int
varsLeft = sum . map numVars
    where
        numVars :: InputShape -> Int
        numVars ShapePV = 2
        numVars ShapeSV = 1
        numVars (RecStmt k is) = k + varsLeft is

-- Given a list of InputShape, give us the number of times we make a new output.
-- That is, this is the number of variables we define.
newVars :: [InputShape] -> Int
newVars = sum . map numVars
    where
        numVars :: InputShape -> Int
        numVars ShapePV = 1
        numVars ShapeSV = 1
        numVars (RecStmt k is) = k + newVars is

-- Take total length and rec length, and generate the corresponding input shape.
generateInputShapes :: Int -> Int -> Gen [InputShape]
-- We enforce that recLen > 1, since we need the loop to do something and have at least one loop variable.
generateInputShapes tot 0 = generateNoRecShapes tot
generateInputShapes tot 1 = generateNoRecShapes tot
generateInputShapes tot recLen = do
    let noRecLen = tot - recLen
    noRecLen1 <- Gen.integral (Range.linear 0 (noRecLen-1))
    let noRecLen2 = noRecLen - noRecLen1
    loopedVars <- Gen.integral (Range.linear (min 2 (recLen-1)) (recLen-1))
    prerec <- generateNoRecShapes noRecLen1
    rec_ <- RecStmt loopedVars <$> generateNoRecShapes (recLen-loopedVars)
    postrec <- generateNoRecShapes noRecLen2
    return $ prerec ++ (rec_ : postrec)

generateNoRecShapes :: Int -> Gen [InputShape]
generateNoRecShapes i = Gen.list (Range.singleton i) $
    Gen.choice [return ShapeSV, return ShapePV]

generateProcCode :: Int -> Int -> Gen ProcCode
generateProcCode len recLen = do
    inpShapes <- generateInputShapes len recLen
    (lines_, VS unused _ _) <- genLines (defaultVarSet inpShapes) inpShapes 1
    let outvar = Set.elemAt 0 unused
    return $ PC lines_ outvar

-- Given a VarSet and a number of lines left to use, generate a var.
someVar :: VarSet -> Gen (Var, VarSet)
someVar vs@(VS _ _ repeatsAllowed)
    -- If we have more variables left than the number of unused variables,
    -- plus the number of unused variables we are going to generate (since we
    -- generate one on each line), we can reuse a variable.
    | repeatsAllowed > 0 = Gen.choice [consumeUnused vs, sampleUsed vs]
    -- Otherwise, we must use an unused one.
    | otherwise = consumeUnused vs

-- Consumes the lowest element of that set.
-- This is so that it is very likely that loop variables are used before the end of the loop.
-- (So that we actually end up with a loop.)
consumeUnused :: VarSet -> Gen (Var, VarSet)
consumeUnused vs@(VS unused used repeatsAllowed)
    | Set.null unused = sampleUsed vs
    | otherwise = let
        e = Set.elemAt 0 unused
        in return (e, VS (Set.delete e unused) (Set.insert e used) repeatsAllowed)

sampleUsed :: VarSet -> Gen (Var, VarSet)
sampleUsed vs@(VS unused used repeatsAllowed)
    | Set.size used == 0 = consumeUnused vs
    | otherwise = do
        e <- Gen.element (Set.toList used)
        return (e, VS unused used (repeatsAllowed-1))

-- Given a VarSet, a list of InputShape to follow, and a line number, generate one line per InputShape with an input of that shape.
genLines :: VarSet -> [InputShape] -> Int -> Gen ([Line], VarSet)
genLines vs [] _ = return ([], vs)
genLines vs (ShapeSV : ss) n = do
    preval <- Gen.double (Range.exponentialFloat 0 100)
    (line, vs') <- Gen.choice [unopGen (Pre preval) vs n, unopGen Inc vs n]
    (lines_, vs'') <- genLines vs' ss (n+1)
    return (line : lines_, vs'')
genLines vs (ShapePV : ss) n = do
    (line, vs') <- Gen.choice [binopGen Add vs n, binopGen Sub vs n, binopGen Mul vs n]
    (lines_, vs'') <- genLines vs' ss (n+1)
    return (line : lines_, vs'')
genLines vs (RecStmt k is : ss) n = do
    -- First, generate k new variables. We give these names n to n+k-1.
    let loopVars = [n..(n+k-1)]
        vs' = vs {unused = Set.union (unused vs) (Set.fromAscList loopVars)}
    -- At the end of generating code for is, we will add on pres that define these vars.
    -- Generate the code for the rest of the lines, starting with n+k. This can use vars n..(n+k-1).
    (reclines, vs'') <- genLines vs' is (n+k)
    -- Now we generate the required pre instructions.
    -- For each loop variable...
    (prelines, vs''') <- foldM (\(lines_, vs) var -> do
            -- Generate Pre preval (some variable) (this variable)
            -- That is, generate a Pre which sets the value of that variable.
            preval <- Gen.double (Range.exponentialFloat 0 100)
            (preinp, vs') <- someVar vs
            return (lines_ ++ [Pre preval preinp var], vs')
        ) ([], vs'') loopVars
    -- Finally, generate the lines for ss.
    (lines_, vs'''') <- genLines vs''' ss (n + length is + k)
    return (RecLine (reclines ++ prelines) : lines_, vs'''')

unopGen :: (SingleVar -> SingleVar -> Line) -> VarSet -> Int -> Gen (Line, VarSet)
unopGen constr vs n = do
    (var, vs') <- someVar vs
    let vs'' = addToUnused vs' n
    return (constr var n, vs'')

binopGen :: (PairVar -> SingleVar -> Line) -> VarSet -> Int -> Gen (Line, VarSet)
binopGen constr vs n = do
    (varl, vs') <- someVar vs
    (varr, vs'') <- someVar vs'
    let vs''' = addToUnused vs'' n
    return (constr (varl, varr) n, vs''')

-- Add to unused if it is not already in used (so if it is a new variable).
-- We need this for generating rec, where the line that generates some variable is
-- later than its use.
addToUnused :: VarSet -> Var -> VarSet
addToUnused vs@(VS unused used repeatsAllowed) v
    | Set.member v used = vs
    | otherwise = VS (Set.insert v unused) used repeatsAllowed