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

type ReadCells :: forall (ar :: Arity). Desc' ar -> MemState -> Constraint
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

type WriteCells :: forall (ar :: Arity). Desc' ar -> MemState -> Constraint
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

type WriteCellsAfter :: forall (ar :: Arity). Desc' ar -> MemState -> Constraint
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

type AsMemory :: forall (ar :: Arity) (br :: Arity).
    Arrow Desc' ar br -> Desc' ar -> Desc' br -> [*] -> [*] -> Constraint
class AsMemory arr a b env prog | arr a b env -> prog where
    toProgram :: AFRP' arr a b -> Proxy a -> HList env -> IO (HList prog, Proxy b)

instance AsMemory ArrowId a a env '[] where
    toProgram Id' prox _ = Prelude.return (HNil, prox)

instance AsMemory ArrowDropL (PN a b) b env '[] where
    toProgram DropL' prox _ = let (_, br) = splitProx prox in Prelude.return (HNil, br)

instance AsMemory ArrowDropR (PN a b) a env '[] where
    toProgram DropR' prox _ = let (ar, _) = splitProx prox in Prelude.return (HNil, ar)

instance AsMemory ArrowDup a (PN a a) env '[] where
    toProgram Dup' prox _ = Prelude.return (HNil, pairProx prox prox)

instance (ReadCells a ar, WriteCells b br,
    MemoryInv ar br, prog ~ '[Memory IO (MemoryPlus ar br) ()]) =>
    AsMemory ArrowArr a b env prog where
        toProgram (Arr' f) inprox _ = do
            let outprox = Proxy :: Proxy b
                comp = (f <$> readCells inprox) Rearrange.>>= writeCells outprox
            Prelude.return (comp :+: HNil, outprox)

instance (ReadCells a ar, WriteCellsAfter a' ar', AsDesc a' ~ AsDesc a,
    MemoryInv ar ar', mems ~ MemoryPlus ar ar', ProxToRef a' env) =>
    AsMemory ArrowPre a a' env '[Memory IO mems ()] where
        toProgram (Pre' v) inprox env = do
            let outprox = Proxy :: Proxy a'
                comp = readCells inprox Rearrange.>>= writeCellsAfter outprox
            flip writeRef v $ proxToRef (Proxy :: Proxy a') env
            Prelude.return (comp :+: HNil, outprox)

instance (AsMemory larr a b env progl, AsMemory rarr b c env progr,
    prog ~ Combine progl progr) =>
    AsMemory (ArrowGGG larr rarr b) a c env prog where
        toProgram (f :>>>:: g) inprox env = do
            (compf, midprox) <- toProgram f inprox env
            (compg, outprox) <- toProgram g midprox env
            Prelude.return (hCombine compf compg, outprox)

instance (AsMemory larr la lb env progl, AsMemory rarr ra rb env progr,
    prog ~ Combine progl progr) =>
    AsMemory (ArrowSSS larr rarr) (PN la ra) (PN lb rb) env prog where
        toProgram (f :****: g) prox env = do
            let (inl, inr) = splitProx prox
            (compf, outl) <- toProgram f inl env
            (compg, outr) <- toProgram g inr env
            Prelude.return (hCombine compf compg, pairProx outl outr)

instance (AsMemory arr (PN a c) (PN b c) env prog) =>
    AsMemory (ArrowLoop arr c) a b env prog where
        toProgram (Loop' f) inref env = do
            (comp, prox') <- toProgram f (pairProx inref (Proxy :: Proxy c)) env
            let (out, _) = splitProx prox'
            Prelude.return (comp, out)

-- The output cell of a pre will always contain the _next_ value, since it is written at the end of each run
-- through writeCellsAfter (this is needed to break the loop - we think of this as the pre preloading the next
-- value to return).
-- This means that if have the output cell of a pre as the output of the entire program, reading it will give
-- us the _next_ value being returned, not the current one.
-- We fix this by adding one separate output cell by postcomposing >>> arr id to the input.
type Augment :: [*] -> Desc' a -> FreshState -> [*] -> Desc' a -> FreshState -> Constraint
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