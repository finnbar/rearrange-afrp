{-# LANGUAGE UndecidableInstances, QualifiedDo, FunctionalDependencies, ScopedTypeVariables #-}

module MakeMIO where

import Data.Type.HList

import AFRP
import Naming

import Rearrange
import Data.Memory.Types (MemState, MemoryPlus)
import Data.Memory.Memory (MemoryInv)
import Data.Kind (Constraint)
import Data.Proxy

type ReadCells :: forall (ar :: SKind). DescAnn ar -> MemState -> Constraint
class ReadCells ac res | ac -> res where
    readCells :: Proxy ac -> Memory IO res (Val (AsDesc ac))

instance ReadCells (VN n a) '( '[], '[IOCell n a], '[]) where
    readCells _ = One <$> readIOCell

instance (ReadCells lc lr, ReadCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => ReadCells (PN lc rc) res where
    readCells prox = Rearrange.do
        let (lp, rp) = splitProx prox
        left <- readCells lp
        Pair left <$> readCells rp

type WriteCells :: forall (ar :: SKind). DescAnn ar -> MemState -> Constraint
class WriteCells ac res | ac -> res where
    writeCells :: Proxy ac -> Val (AsDesc ac) -> Memory IO res ()

instance WriteCells (VN n a) '( '[IOCell n a], '[], '[]) where
    writeCells _ (One v) = writeIOCell v

instance (WriteCells lc lr, WriteCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => WriteCells (PN lc rc) res where
    writeCells prox (Pair a b) = Rearrange.do
        let (l, r) = splitProx prox
        writeCells l a
        writeCells r b

type WriteCellsAfter :: forall (ar :: SKind). DescAnn ar -> MemState -> Constraint
class WriteCellsAfter ac res | ac -> res where
    writeCellsAfter :: Proxy ac -> Val (AsDesc ac) -> Memory IO res ()

instance WriteCellsAfter (VN n a) '( '[], '[], '[IOCell n a]) where
    writeCellsAfter _ (One v) = writeIOCellAfter v

instance (WriteCellsAfter lc lr, WriteCellsAfter rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => WriteCellsAfter (PN lc rc) res where
    writeCellsAfter prox (Pair a b) = Rearrange.do
        let (l, r) = splitProx prox
        writeCellsAfter l a
        writeCellsAfter r b

class ProxToRef ac env where
    proxToRef :: Proxy ac -> HList env -> Ref ac

instance (LookupNth n env (IOCell n a)) => ProxToRef (VN n a) env where
    proxToRef Proxy env =
        let cell = lookupNth (Proxy :: Proxy n) env in VRef cell

instance (ProxToRef l env, ProxToRef r env) => ProxToRef (PN l r) env where
    proxToRef prox env = let (pl, pr) = splitProx prox in
        PRef (proxToRef pl env) (proxToRef pr env)

-- Since the type of the Memory computation as output is going to differ based on which
-- Arrow combinator we use, we need a different case for each combinator.

-- NOTE: We cannot do this by having lowercase arr etc. instead of transforming a special GADT.
-- This is because the types get very difficult for a user to specify: 
-- If we just have e.g. lowercase arr :: (Val a -> Val b) -> Ref ans a -> IO (HList prog, Ref bns b)
-- then a user will have to type `arr f bs rf` with the correct fs' and prog.
-- There might be a world where a user can say that the type is (Arr <a bunch of arguments>) but that seems worse than the current one.

type ToMIO :: forall (ar :: SKind) (br :: SKind).
    AFRPConAnn ar br -> DescAnn ar -> DescAnn br -> [*] -> Constraint
class ToMIO arr a b prog | arr a b -> prog where
    toMIO :: AFRPAnn arr a b -> Proxy a -> IO (HList prog, Proxy b)

instance ToMIO ArrowId' a a '[] where
    toMIO Id' prox = Prelude.return (HNil, prox)

instance ToMIO ArrowDropL' (PN a b) b '[] where
    toMIO DropL' prox = let (_, br) = splitProx prox in Prelude.return (HNil, br)

instance ToMIO ArrowDropR' (PN a b) a '[] where
    toMIO DropR' prox = let (ar, _) = splitProx prox in Prelude.return (HNil, ar)

instance ToMIO ArrowDup' a (PN a a) '[] where
    toMIO Dup' prox = Prelude.return (HNil, pairProx prox prox)

instance ToMIO (ArrowConst' a) x a '[] where
    toMIO (Constant' c) _ = do
        let outprox = Proxy :: Proxy a
        Prelude.return (HNil, outprox)

instance (ReadCells a ar, WriteCells b br,
    MemoryInv ar br, prog ~ '[Memory IO (MemoryPlus ar br) ()]) =>
    ToMIO ArrowArr' a b prog where
        toMIO (Arr' f) inprox = do
            let outprox = Proxy :: Proxy b
                comp = (f <$> readCells inprox) Rearrange.>>= writeCells outprox
            Prelude.return (comp :+: HNil, outprox)

instance (ReadCells a ar, WriteCellsAfter a' ar', AsDesc a' ~ AsDesc a,
    MemoryInv ar ar', mems ~ MemoryPlus ar ar') =>
    ToMIO ArrowPre' a a' '[Memory IO mems ()] where
        toMIO (Pre' v) inprox = do
            let outprox = Proxy :: Proxy a'
                comp = readCells inprox Rearrange.>>= writeCellsAfter outprox
            Prelude.return (comp :+: HNil, outprox)

instance (ToMIO larr a b progl, ToMIO rarr b c progr,
    prog ~ Combine progl progr) =>
    ToMIO (ArrowGGG' larr rarr b) a c prog where
        toMIO (f :>>>:: g) inprox = do
            (compf, midprox) <- toMIO f inprox
            (compg, outprox) <- toMIO g midprox
            Prelude.return (hCombine compf compg, outprox)

instance (ToMIO larr la lb progl, ToMIO rarr ra rb progr,
    prog ~ Combine progl progr) =>
    ToMIO (ArrowSSS' larr rarr) (PN la ra) (PN lb rb) prog where
        toMIO (f :***:: g) prox = do
            let (inl, inr) = splitProx prox
            (compf, outl) <- toMIO f inl
            (compg, outr) <- toMIO g inr
            Prelude.return (hCombine compf compg, pairProx outl outr)

instance (ToMIO arr (PN a c) (PN b c) prog) =>
    ToMIO (ArrowLoop' arr c) a b prog where
        toMIO (Loop' f) inref = do
            (comp, prox') <- toMIO f (pairProx inref (Proxy :: Proxy c))
            let (out, _) = splitProx prox'
            Prelude.return (comp, out)

-- The output cell of a pre will always contain the _next_ value, since it is written at the end of each run
-- through writeCellsAfter (this is needed to break the loop - we think of this as the pre preloading the next
-- value to return).
-- This means that if have the output cell of a pre as the output of the entire program, reading it will give
-- us the _next_ value being returned, not the current one.
-- We fix this by adding one separate output cell by postcomposing >>> arr id to the input.
type Augment :: [*] -> DescAnn a -> FreshState -> [*] -> DescAnn a -> FreshState -> Constraint
class Augment prog a bs prog' a' bs' | prog a bs -> prog' a' bs' where
    augment :: HList prog -> Proxy a -> BuildState bs -> IO (HList prog', Proxy a', BuildState bs')

instance forall a a' fs fs' read write prog prog'.
    (Fresh (AsDesc a) fs a' fs',
    ReadCells a read, WriteCells a' write,
    MemoryInv read write,
    AsDesc a ~ AsDesc a',
    prog' ~ Append (MIO (MemoryPlus read write) ()) prog) =>
    Augment prog a fs prog' a' fs' where
        augment prog prox bs = do
            (prox', bs') <- fresh bs (Proxy :: Proxy (AsDesc a))
            let comp = readCells prox Rearrange.>>= writeCells prox'
            Prelude.return (hAppend comp prog, prox', bs')