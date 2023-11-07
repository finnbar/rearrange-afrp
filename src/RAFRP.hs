{-# LANGUAGE UndecidableInstances, QualifiedDo, FunctionalDependencies, ScopedTypeVariables #-}

module RAFRP (Ref(..), Desc(..), Val(..), AFRP(..), makeAFRP, makeRunnable) where

import Data.Type.HList

import AFRP

import Rearrange
import Data.Memory.Types (MemState, MemoryPlus)
import Data.Memory.Memory (MemoryInv)
import Data.Type.Utils
import Data.Kind (Constraint)
import Data.Type.Set

-- * Data types and constructors

-- In order to generate fresh type variables, we need a state to track which vars are unused.
-- Vars are Nat, so this stores the highest number used so far.
-- (The program is created by setting BuildState to 0.)

type ReadCells :: forall (ar :: Arity). Desc' ar -> MemState -> Constraint
class ReadCells ac res | ac -> res where
    readCells :: DescProxy' ac -> Memory IO res (Val (AsDesc ac))

instance ReadCells (VN (Just n) a) '( '[], '[IOCell n a], '[]) where
    readCells _ = One <$> readIOCell

instance (ReadCells lc lr, ReadCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => ReadCells (PN lc rc) res where
    readCells (PProx lp rp) = Rearrange.do
        left <- readCells lp
        Pair left <$> readCells rp

type WriteCells :: forall (ar :: Arity). Desc' ar -> MemState -> Constraint
class WriteCells ac res | ac -> res where
    writeCells :: DescProxy' ac -> Val (AsDesc ac) -> Memory IO res ()

instance WriteCells (VN (Just n) a) '( '[IOCell n a], '[], '[]) where
    writeCells _ (One v) = writeIOCell v

instance (WriteCells lc lr, WriteCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => WriteCells (PN lc rc) res where
    writeCells (PProx l r) (Pair a b) = Rearrange.do
        writeCells l a
        writeCells r b

type WriteCellsAfter :: forall (ar :: Arity). Desc' ar -> MemState -> Constraint
class WriteCellsAfter ac res | ac -> res where
    writeCellsAfter :: DescProxy' ac -> Val (AsDesc ac) -> Memory IO res ()

instance WriteCellsAfter (VN (Just n) a) '( '[], '[], '[IOCell n a]) where
    writeCellsAfter _ (One v) = writeIOCellAfter v

instance (WriteCellsAfter lc lr, WriteCellsAfter rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => WriteCellsAfter (PN lc rc) res where
    writeCellsAfter (PProx l r) (Pair a b) = Rearrange.do
        writeCellsAfter l a
        writeCellsAfter r b

-- Since the type of the Memory computation as output is going to differ based on which
-- Arrow combinator we use, we need a different case for each combinator.

-- NOTE: We cannot do this by having lowercase arr etc. instead of transforming a special GADT.
-- This is because the types get very difficult for a user to specify: 
-- If we just have e.g. lowercase arr :: (Val a -> Val b) -> Ref ans a -> IO (HList prog, Ref bns b)
-- then a user will have to type `arr f bs rf` with the correct fs' and prog.
-- There might be a world where a user can say that the type is (Arr <a bunch of arguments>) but that seems worse than the current one.

type AsMemory :: forall (ar :: Arity) (br :: Arity).
    Arrow Desc' ar br -> Desc' ar -> Desc' br -> [*] -> Constraint
class AsMemory arr a b prog | arr a b -> prog where
    toProgram :: AFRP' arr a b -> DescProxy' a -> (HList prog, DescProxy' b)

instance AsMemory ArrowId a a '[] where
    toProgram Id' rf = (HNil, rf)

instance AsMemory ArrowDropL (PN a b) a '[] where
    toProgram DropL' (PProx ar _) = (HNil, ar)

instance AsMemory ArrowDropR (PN a b) b '[] where
    toProgram DropR' (PProx _ br) = (HNil, br)

instance AsMemory ArrowDup a (PN a a) '[] where
    toProgram Dup' ar = (HNil, PProx ar ar)

instance (ReadCells a ar, WriteCells b br, MakeDescProxy' b,
    MemoryInv ar br, prog ~ '[Memory IO (MemoryPlus ar br) ()]) =>
    AsMemory ArrowArr a b prog where
        toProgram (Arr' f) inprox =
            let outprox = makeDescProxy' @b
                comp = (f <$> readCells inprox) Rearrange.>>= writeCells outprox
            in (comp :+: HNil, outprox)

instance (MakeDescProxy' a', ReadCells a ar, WriteCellsAfter a' ar', AsDesc a' ~ AsDesc a,
    MemoryInv ar ar', mems ~ MemoryPlus ar ar') =>
    AsMemory ArrowPre a a' '[Memory IO mems ()] where
        toProgram (Pre' _) inprox =
            let outprox = makeDescProxy' @a'
                comp = readCells inprox Rearrange.>>= writeCellsAfter outprox
            in (comp :+: HNil, outprox)

instance (AsMemory larr a b progl, AsMemory rarr b c progr,
    prog ~ Combine progl progr) =>
    AsMemory (ArrowGGG larr rarr b) a c prog where
        toProgram (f :>>>>: g) inprox =
            let (compf, midprox) = toProgram f inprox
                (compg, outprox) = toProgram g midprox
            in (hCombine compf compg, outprox)

instance (AsMemory larr la lb progl, AsMemory rarr ra rb progr,
    prog ~ Combine progl progr) =>
    AsMemory (ArrowSSS larr rarr) (PN la ra) (PN lb rb) prog where
        toProgram (f :****: g) (PProx inl inr) =
            let (compf, outl) = toProgram f inl
                (compg, outr) = toProgram g inr
            in (hCombine compf compg, PProx outl outr)

instance (MakeDescProxy' c, AsMemory arr (PN a c) (PN b c) prog) =>
    AsMemory (ArrowLoop arr c) a b prog where
        toProgram (Loop' f) inref =
            let (comp, PProx out _) = toProgram f (PProx inref (makeDescProxy' @c))
            in (comp, out)

data CompiledAFRP (env :: [*]) (prog :: [*]) (a :: Desc' x) (b :: Desc' y) =
    CAFRP (Set env) (HList prog) (Ref a) (Ref b)

-- TODO Reimplement this. Might be part of Naming?
-- The output cell of a pre will always contain the _next_ value, since it is written at the end of each run
-- through writeCellsAfter (this is needed to break the loop - we think of this as the pre preloading the next
-- value to return).
-- This means that if have the output cell of a pre as the output of the entire program, reading it will give
-- us the _next_ value being returned, not the current one.
-- We fix this by adding one separate output cell, which the current output is copied to before any call to
-- writeCellsAfter (just by using regular readCell >>= writeCell).
-- TODO can we avoid this somehow with a callback that runs before any WriteAfter?
-- Can we simplify WriteAfter altogether by returning it as an IO action, since ordering doesn't matter as long
-- as it is run last?

-- type AddSeparateOutput fs prog ans a fs' prog' ans' copy = (Fresh a fs ans' fs', CopyCells a ans ans' copy, prog' ~ Combine prog copy)
-- addSeparateOutput :: AddSeparateOutput fs prog ans a fs' prog' ans' copy
--     => (BuildState fs, HList prog, Ref ans a) -> IO (BuildState fs', HList prog', Ref ans' a)
-- addSeparateOutput (st, prog, inref) = do
--     (outref, st') <- fresh st
--     let copyAns = copyCells inref outref
--     Prelude.return (st', hCombine prog copyAns, outref)

-- makeAFRP :: (Fresh a '(0, '[]) ans fs, AsMemory arr a b ans fs bns fs' prog,
--     AddSeparateOutput fs' prog bns b '(n', env') prog' bns' copy,
--     Sortable env', Nubable (Sort env'))
--     => AFRP arr a b -> IO (CompiledAFRP (AsSet env') prog' a b ans bns')
-- makeAFRP afrp = do
--     (inref, st) <- fresh (MkBuildState HNil :: BuildState '(0, '[]))
--     res <- toProgram afrp st inref
--     (MkBuildState env, prog, outref) <- addSeparateOutput res
--     Prelude.return $ CAFRP (hlistToSet env) prog inref outref

-- TODO Likely promote this from the stuff in Naming, because this relies on the typeclasses in there.
makeAFRP = undefined

hlistToSet :: (Sortable xs, Nubable (Sort xs)) => HList xs -> Set (AsSet xs)
hlistToSet s = asSet (hls s)
    where
        hls :: HList xs -> Set xs
        hls HNil = Empty
        hls (x :+: xs) = Ext x (hls xs)

makeRunnable :: (MakeProgConstraints prog prog' env, RunMems_ IO prog' env) =>
    CompiledAFRP env prog a b -> IO (Val (AsDesc a) -> IO (Val (AsDesc b)))
makeRunnable (CAFRP env mems inref outref) = do
    prog <- makeProgram mems env
    Prelude.return $ \inp -> do
        writeRef inref inp
        runProgram_ prog
        readRef outref
