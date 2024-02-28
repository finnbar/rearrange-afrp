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

type ReadCells :: forall (sk :: SKind). DescAnn sk -> MemState -> Constraint
class ReadCells descAnn grade | descAnn -> grade where
    readCells :: Proxy descAnn -> Memory IO grade (Val (AsDesc descAnn))

instance ReadCells (VN n a) '( '[], '[IOCell n a], '[]) where
    readCells _ = One <$> readIOCell

instance (ReadCells descL gradeL, ReadCells descR gradeR, res ~ MemoryPlus gradeL gradeR,
    MemoryInv gradeL gradeR) => ReadCells (PN descL descR) res where
    readCells prox = Rearrange.do
        let (lp, rp) = splitProx prox
        left <- readCells lp
        Pair left <$> readCells rp

type WriteCells :: forall (sk :: SKind). DescAnn sk -> MemState -> Constraint
class WriteCells descAnn grade | descAnn -> grade where
    writeCells :: Proxy descAnn -> Val (AsDesc descAnn) -> Memory IO grade ()

instance WriteCells (VN n a) '( '[IOCell n a], '[], '[]) where
    writeCells _ (One v) = writeIOCell v

instance (WriteCells descL gradeL, WriteCells descR gradeR, res ~ MemoryPlus gradeL gradeR,
    MemoryInv gradeL gradeR) => WriteCells (PN descL descR) res where
    writeCells prox (Pair a b) = Rearrange.do
        let (l, r) = splitProx prox
        writeCells l a
        writeCells r b

type WriteCellsAfter :: forall (sk :: SKind). DescAnn sk -> MemState -> Constraint
class WriteCellsAfter descAnn grade | descAnn -> grade where
    writeCellsAfter :: Proxy descAnn -> Val (AsDesc descAnn) -> Memory IO grade ()

instance WriteCellsAfter (VN n a) '( '[], '[], '[IOCell n a]) where
    writeCellsAfter _ (One v) = writeIOCellAfter v

instance (WriteCellsAfter descL gradeL, WriteCellsAfter descR gradeR, res ~ MemoryPlus gradeL gradeR,
    MemoryInv gradeL gradeR) => WriteCellsAfter (PN descL descR) res where
    writeCellsAfter prox (Pair a b) = Rearrange.do
        let (l, r) = splitProx prox
        writeCellsAfter l a
        writeCellsAfter r b

class ProxToRef descAnn env where
    proxToRef :: Proxy descAnn -> HList env -> Ref descAnn

instance (LookupNth n env (IOCell n a)) => ProxToRef (VN n a) env where
    proxToRef Proxy env =
        let cell = lookupNth (Proxy :: Proxy n) env in VRef cell

instance (ProxToRef descL env, ProxToRef descR env) => ProxToRef (PN descL descR) env where
    proxToRef prox env = let (pl, pr) = splitProx prox in
        PRef (proxToRef pl env) (proxToRef pr env)

-- Since the type of the Memory computation as output is going to differ based on which
-- Arrow combinator we use, we need a different case for each combinator.

-- NOTE: We cannot do this by having lowercase arr etc. instead of transforming a special GADT.
-- This is because the types get very difficult for a user to specify: 
-- If we just have e.g. lowercase arr :: (Val a -> Val b) -> Ref ans a -> IO (HList prog, Ref bns b)
-- then a user will have to type `arr f bs rf` with the correct fs' and prog.
-- There might be a world where a user can say that the type is (Arr <a bunch of arguments>) but that seems worse than the current one.

type ToMIO :: forall (sk :: SKind) (sk' :: SKind).
    ConAnn sk sk' -> DescAnn sk -> DescAnn sk' -> [*] -> Constraint
class ToMIO conAnn inpAnn outAnn mios | conAnn inpAnn outAnn -> mios where
    toMIO :: SFAnn conAnn inpAnn outAnn -> Proxy inpAnn -> IO (HList mios, Proxy outAnn)

instance ToMIO IdCon' a a '[] where
    toMIO Id' prox = Prelude.return (HNil, prox)

instance ToMIO DropLCon' (PN a b) b '[] where
    toMIO DropL' prox = let (_, br) = splitProx prox in Prelude.return (HNil, br)

instance ToMIO DropRCon' (PN a b) a '[] where
    toMIO DropR' prox = let (ar, _) = splitProx prox in Prelude.return (HNil, ar)

instance ToMIO DupCon' a (PN a a) '[] where
    toMIO Dup' prox = Prelude.return (HNil, pairProx prox prox)

instance ToMIO (ConstCon' a) x a '[] where
    toMIO (Constant' c) _ = do
        let outprox = Proxy :: Proxy a
        Prelude.return (HNil, outprox)

instance (ReadCells inpAnn read, WriteCells outAnn write,
    MemoryInv read write, prog ~ '[MIO (MemoryPlus read write) ()]) =>
    ToMIO ArrCon' inpAnn outAnn prog where
        toMIO (Arr' f) inprox = do
            let outprox = Proxy :: Proxy outAnn
                comp = (f <$> readCells inprox) Rearrange.>>= writeCells outprox
            Prelude.return (comp :+: HNil, outprox)

instance (ReadCells inpAnn read, WriteCellsAfter outAnn after, AsDesc outAnn ~ AsDesc inpAnn,
    MemoryInv read after, mems ~ MemoryPlus read after) =>
    ToMIO PreCon' inpAnn outAnn '[Memory IO mems ()] where
        toMIO (Pre' v) inprox = do
            let outprox = Proxy :: Proxy outAnn
                comp = readCells inprox Rearrange.>>= writeCellsAfter outprox
            Prelude.return (comp :+: HNil, outprox)

instance (ToMIO conAnn1 inpAnn midAnn prog1, ToMIO conAnn2 midAnn outAnn prog2,
    prog ~ Combine prog1 prog2) =>
    ToMIO (GGGCon' conAnn1 conAnn2 midAnn) inpAnn outAnn prog where
        toMIO (f :>>>:: g) inprox = do
            (compf, midprox) <- toMIO f inprox
            (compg, outprox) <- toMIO g midprox
            Prelude.return (hCombine compf compg, outprox)

instance (ToMIO conAnn1 inpLAnn outLAnn prog1, ToMIO conAnn2 inpRAnn outRAnn prog2,
    prog ~ Combine prog1 prog2) =>
    ToMIO (SSSCon' conAnn1 conAnn2) (PN inpLAnn inpRAnn) (PN outLAnn outRAnn) prog where
        toMIO (f :***:: g) prox = do
            let (inl, inr) = splitProx prox
            (compf, outl) <- toMIO f inl
            (compg, outr) <- toMIO g inr
            Prelude.return (hCombine compf compg, pairProx outl outr)

instance (ToMIO con (PN inpAnn loopedAnn) (PN outAnn loopedAnn) prog) =>
    ToMIO (LoopCon' con loopedAnn) inpAnn outAnn prog where
        toMIO (Loop' f) inref = do
            (comp, prox') <- toMIO f (pairProx inref (Proxy :: Proxy loopedAnn))
            let (out, _) = splitProx prox'
            Prelude.return (comp, out)

-- The output cell of a pre will always contain the _next_ value, since it is written at the end of each run
-- through writeCellsAfter (this is needed to break the loop - we think of this as the pre preloading the next
-- value to return).
-- This means that if have the output cell of a pre as the output of the entire program, reading it will give
-- us the _next_ value being returned, not the current one.
-- We fix this by adding one separate output cell by postcomposing >>> arr id to the input.
type Augment :: [*] -> DescAnn a -> FreshKind -> [*] -> DescAnn a -> FreshKind -> Constraint
class Augment prog out fk0 prog' out' fk1 | prog out fk0 -> prog' out' fk1 where
    augment :: HList prog -> Proxy out -> FreshState fk0 -> IO (HList prog', Proxy out', FreshState fk1)

instance forall out out' fk0 fk1 read write prog prog'.
    (Fresh (AsDesc out) fk0 out' fk1,
    ReadCells out read, WriteCells out' write,
    MemoryInv read write,
    AsDesc out ~ AsDesc out',
    prog' ~ Append (MIO (MemoryPlus read write) ()) prog) =>
    Augment prog out fk0 prog' out' fk1 where
        augment prog prox bs = do
            (prox', bs') <- fresh bs (Proxy :: Proxy (AsDesc out))
            let comp = readCells prox Rearrange.>>= writeCells prox'
            Prelude.return (hAppend comp prog, prox', bs')