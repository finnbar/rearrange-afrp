{-# LANGUAGE TupleSections #-}

module GenerateProcCode (generateProcCode) where

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Set (Set)
import qualified Data.Set as Set

-- Proc code consists of some number of lines of code and then a numbered Var
data ProcCode = PC [Line] SingleVar
    deriving (Show)
-- Each line is vj <- op -< vs, where j is the line number and vs are the input(s).
-- e.g. Pre v vl vr === vr <- pre v -< vl.
data Line = Pre Int SingleVar SingleVar | Add PairVar SingleVar | Sub PairVar SingleVar
    | Mul PairVar SingleVar | Div PairVar SingleVar | Inc SingleVar SingleVar
    deriving (Show)
-- Ints are used to represent vars, with variable i being called v_i.
type Var = Int
type SingleVar = Var
type PairVar = (Var, Var)
data InputShape = ShapeSV | ShapePV

data VarSet = VS {
    unused :: Set Int,
    used :: Set Int,
    repeatsAllowed :: Int }

defaultVarSet :: [InputShape] -> VarSet
defaultVarSet inps = VS (Set.singleton 0) Set.empty (varsLeft inps - length inps)

-- Given a list of InputShape, give us the number of slots left for variables.
varsLeft :: [InputShape] -> Int
varsLeft [] = 0
varsLeft (ShapePV : ss) = 2 + varsLeft ss
varsLeft (ShapeSV : ss) = 1 + varsLeft ss

generateProcCode :: Int -> Gen ProcCode
generateProcCode len = do
    -- We first generate the number of inputs that each line will consume.
    -- We use this later to make sure that every variable is used at least once.
    inps <- Gen.list (Range.singleton len) $ Gen.choice [return ShapeSV, return ShapePV]
    (lines, VS unused _ _) <- genLines (defaultVarSet inps) inps 1
    return $ PC lines (Set.elemAt 0 unused)

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
consumeUnused :: VarSet -> Gen (Var, VarSet)
consumeUnused vs@(VS unused used repeatsAllowed)
    | Set.size unused == 0 = sampleUsed vs
    | otherwise = let
        e = Set.elemAt 0 unused
        in return (e, VS (Set.drop 1 unused) (Set.insert e used) repeatsAllowed)

sampleUsed :: VarSet -> Gen (Var, VarSet)
sampleUsed vs@(VS unused used repeatsAllowed)
    | Set.size used == 0 = consumeUnused vs
    | otherwise = do
        e <- Gen.element (Set.toList used)
        return (e, VS unused used (repeatsAllowed-1))

-- Given a VarSet, a list of InputShape to follow, and a line number, generate one line per InputShape with an input of that shape.
genLines :: VarSet -> [InputShape] -> Int -> Gen ([Line], VarSet)
genLines vs [] n = return ([], vs)
genLines vs (ShapeSV : ss) n = do
    preval <- Gen.integral (Range.linear 0 100)
    (line, vs') <- Gen.choice [unopGen (Pre preval) vs n, unopGen Inc vs n]
    (lines, vs'') <- genLines vs' ss (n+1)
    return (line : lines, vs'')
genLines vs (ShapePV : ss) n = do
    (line, vs') <- Gen.choice [binopGen Add vs n, binopGen Sub vs n, binopGen Div vs n, binopGen Mul vs n]
    (lines, vs'') <- genLines vs' ss (n+1)
    return (line : lines, vs'')

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

addToUnused :: VarSet -> Var -> VarSet
addToUnused (VS unused used repeatsAllowed) v = VS (Set.insert v unused) used repeatsAllowed