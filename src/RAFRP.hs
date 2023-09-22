{-# LANGUAGE UndecidableInstances, QualifiedDo, FunctionalDependencies, ScopedTypeVariables #-}

module RAFRP (Ref(..), Desc(..), Val(..), AFRP(..), makeAFRP, makeRunnable) where

import GHC.TypeLits
import Data.Type.HList

import Rearrange
import Data.Memory.Types (MemState, MemoryPlus)
import Data.Memory.Memory (MemoryInv)
import Data.Type.Utils
import Data.Kind (Constraint)
import Data.IORef (writeIORef, readIORef, newIORef)
import Data.Type.Set

-- * Data types and constructors

data Arity where
    O :: Arity
    (:::) :: Arity -> Arity -> Arity

type Desc :: Arity -> *
data Desc x where
    V :: forall (a :: *). a -> Desc O
    P :: Desc a -> Desc b -> Desc (a ::: b)

type Names :: Arity -> *
data Names x where
    VN :: Nat -> Names O
    PN :: Names a -> Names b -> Names (a ::: b)

type Val :: forall s. Desc s -> *
data Val x where
    One :: !a -> Val (V a)
    Pair :: !(Val a) -> !(Val b) -> Val (P a b)

instance Show a => Show (Val (V a)) where
    show (One a) = show a

instance (Show (Val l), Show (Val r)) => Show (Val (P l r)) where
    show (Pair l r) = "[|" ++ show l ++ ", " ++ show r ++ "|]"

type Ref :: forall s. Names s -> Desc s -> *
data Ref nam desc where
    VRef :: IOCell n a -> Ref (VN n) (V a)
    PRef :: Ref ln l -> Ref rn r -> Ref (PN ln rn) (P l r)

readRef :: Ref n d -> IO (Val d)
readRef (VRef (Cell i)) = One <$> readIORef i
readRef (PRef l r) = Pair <$> readRef l <*> readRef r

writeRef :: Ref n d -> Val d -> IO ()
writeRef (VRef (Cell i)) (One v) = writeIORef i v
writeRef (PRef l r) (Pair i j) = writeRef l i Prelude.>> writeRef r j

-- Used by Pre to note what defaults some memory cells need (since they are the values for pre at t=0).

-- In order to generate fresh type variables, we need a state to track which vars are unused.
-- Vars are Nat, so this stores the highest number used so far.
-- (The program is created by setting BuildState to 0.)

type FreshState = (Nat, [*])
type BuildState :: FreshState -> *
type family EnvFromBuildState (x :: FreshState) where
    EnvFromBuildState '(n, env) = env
newtype BuildState (fs :: FreshState) = MkBuildState (HList (EnvFromBuildState fs))

type Fresh :: forall s. Desc s -> FreshState -> Names s -> FreshState -> Constraint
class Fresh d fs ns fs' | d fs -> ns fs' where
    fresh :: BuildState fs -> IO (Ref ns d, BuildState fs')

instance (n' ~ n + 1) => Fresh (V (a :: *)) '(n, env) (VN n) '(n', IOCell n a ': env) where
    fresh (MkBuildState ps) = do
        ref <- newIORef undefined
        let cell = Cell @_ @_ @IO ref
        Prelude.return (VRef cell, MkBuildState $ cell :+: ps)

instance (Fresh l fs lns fs', Fresh r fs' rns fs'') =>
    Fresh (P l r) fs (PN lns rns) fs'' where
    fresh st = do
        (l, st') <- fresh st
        (r, st'') <- fresh st'
        Prelude.return (PRef l r, st'')

type ReadCells :: forall (ar :: Arity). Desc ar -> Names ar -> MemState -> Constraint
class ReadCells ac ns res | ac ns -> res where
    readCells :: Ref ns ac -> Memory IO res (Val ac)

instance ReadCells (V a) (VN n) '( '[], '[IOCell n a], '[]) where
    readCells _ = One <$> readIOCell

instance (ReadCells lc ln lr, ReadCells rc rn rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => ReadCells (P lc rc) (PN ln rn) res where
    readCells (PRef lp rp) = Rearrange.do
        left <- readCells lp
        Pair left <$> readCells rp

type WriteCells :: forall (ar :: Arity). Desc ar -> Names ar -> MemState -> Constraint
class WriteCells ac ns res | ac ns -> res where
    writeCells :: Ref ns ac -> Val ac -> Memory IO res ()

instance WriteCells (V a) (VN n) '( '[IOCell n a], '[], '[]) where
    writeCells _ (One v) = writeIOCell v

instance (WriteCells lc ln lr, WriteCells rc rn rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => WriteCells (P lc rc) (PN ln rn) res where
    writeCells (PRef l r) (Pair a b) = Rearrange.do
        writeCells l a
        writeCells r b

type WriteCellsAfter :: forall (ar :: Arity). Desc ar -> Names ar -> MemState -> Constraint
class WriteCellsAfter ac ns res | ac ns -> res where
    writeCellsAfter :: Ref ns ac -> Val ac -> Memory IO res ()

instance WriteCellsAfter (V a) (VN n) '( '[], '[], '[IOCell n a]) where
    writeCellsAfter _ (One v) = writeIOCellAfter v

instance (WriteCellsAfter lc ln lr, WriteCellsAfter rc rn rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => WriteCellsAfter (P lc rc) (PN ln rn) res where
    writeCellsAfter (PRef l r) (Pair a b) = Rearrange.do
        writeCellsAfter l a
        writeCellsAfter r b

type CopyCells :: forall (ar :: Arity). Desc ar -> Names ar -> Names ar -> [*] -> Constraint
class CopyCells d ns ns' prog | d ns ns' -> prog where
    copyCells :: Ref ns d -> Ref ns' d -> HList prog

instance (ReadCells (V a) (VN n) resl, WriteCells (V a) (VN n') resr, res ~ MemoryPlus resl resr, MemoryInv resl resr)
    => CopyCells (V a) (VN n) (VN n') '[Memory IO res ()] where
        copyCells ra ra' =
            let comp = readCells ra Rearrange.>>= writeCells ra'
            in comp :+: HNil

instance (CopyCells l ln ln' resl, CopyCells r rn rn' resr, res ~ Combine resl resr)
    => CopyCells (P l r) (PN ln rn) (PN ln' rn') res where
        copyCells (PRef ra ra') (PRef rb rb') =
            let compl = copyCells ra rb
                compr = copyCells ra' rb'
            in hCombine compl compr

-- Programmers write their code in this intermediate GADT.
-- This is so that programmers can write programs without worrying about names.
-- This should allow full inference: assume we have a well-typed AFRP, then we transform to well-typed RAFRP,
-- which can then be (possibly) implemented with rearrange.

type Arrow :: Arity -> Arity -> *
data Arrow ar ar' where
    ArrowId :: Arrow a a
    ArrowDropL :: Arrow (a ::: b) b
    ArrowDropR :: Arrow (a ::: b) a
    ArrowDup :: Arrow a (a ::: a)
    ArrowArr :: Arrow a b
    ArrowPre :: Arrow a b
    ArrowGGG :: Arrow a b -> Arrow b c -> Desc b -> Arrow a c
    ArrowSSS :: Arrow a b -> Arrow a' b' -> Arrow (a ::: a') (b ::: b')
    ArrowLoop :: Arrow (a ::: c) (b ::: c) -> Desc c -> Arrow a b

type ArityFromDesc :: forall s. Desc s -> Arity
type family ArityFromDesc d where
    ArityFromDesc (V a) = O
    ArityFromDesc (P l r) = ArityFromDesc l ::: ArityFromDesc r

-- Programmers will likely use :: AFRP _ a b in their code, since _ is entirely inferrable from the constructors.
-- We need it to implement an AFRP -> RAFRP function, as the type of the output cannot solely depend on the value of the input.
-- (That's dependent types innit.)
type AFRP :: forall (ar :: Arity) (ar' :: Arity). Arrow ar ar' -> Desc ar -> Desc ar' -> *
data AFRP arrow a b where
    -- Routing
    Id :: AFRP ArrowId a a
    DropL :: AFRP ArrowDropL (P a b) b
    DropR :: AFRP ArrowDropR (P a b) a
    Dup :: AFRP ArrowDup a (P a a)
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr :: (Val a -> Val b) -> AFRP ArrowArr a b
    Pre :: Val a -> AFRP ArrowPre a a
    (:>>>:) :: AFRP ar a b -> AFRP ar' b c -> AFRP (ArrowGGG ar ar' b) a c
    (:***:) :: AFRP ar a b -> AFRP ar' a' b' -> AFRP (ArrowSSS ar ar') (P a a') (P b b')
    Loop :: AFRP ar (P a c) (P b c) -> AFRP (ArrowLoop ar c) a b

-- Since the type of the Memory computation as output is going to differ based on which
-- Arrow combinator we use, we need a type family to guide the relevant type inference.
-- Given the arrow combinator, its input and output types, its input name and BuildState,
-- we need the output name, BuildState and the resulting program.

-- HOW TO READ THE TYPE VARIABLES
-- ns suffix => names, e.g. xns is names of x.
-- fs = FreshState, used for carrying around the state needed by Fresh.
-- prog = the Memory program being generated.
-- arr = the type-level representation of the arrow program itself. Also used as a suffix.

type AsMemory :: forall (ar :: Arity) (br :: Arity).
    Arrow ar br -> Desc ar -> Desc br -> Names ar -> FreshState -> Names br -> FreshState -> [*] -> Constraint
class AsMemory arr a b ans fs bns fs' prog | arr a b ans fs -> bns fs' prog where
    toProgram :: AFRP arr a b -> BuildState fs -> Ref ans a -> IO (BuildState fs', HList prog, Ref bns b)

instance AsMemory ArrowId a a ans fs ans fs '[] where
    toProgram Id st rf = Prelude.return (st, HNil, rf)

instance AsMemory ArrowDropL (P a b) a (PN ans bns) fs ans fs '[] where
    toProgram DropL st (PRef ar _) = Prelude.return (st, HNil, ar)

instance AsMemory ArrowDropR (P a b) b (PN ans bns) fs bns fs '[] where
    toProgram DropR st (PRef _ br) = Prelude.return (st, HNil, br)

instance AsMemory ArrowDup a (P a a) ans fs (PN ans ans) fs '[] where
    toProgram Dup st ar = Prelude.return (st, HNil, PRef ar ar)

instance (Fresh b fs bns fs', ReadCells a ans ar, WriteCells b bns br,
    MemoryInv ar br, prog ~ '[Memory IO (MemoryPlus ar br) ()]) =>
    AsMemory ArrowArr a b ans fs bns fs' prog where
        toProgram (Arr f) st inref = do
            (outref, st') <- fresh st
            let comp = (f <$> readCells inref) Rearrange.>>= writeCells outref
            Prelude.return (st', comp :+: HNil, outref)

instance (Fresh a fs ans' fs', ReadCells a ans ar, WriteCellsAfter a ans' ar', MemoryInv ar ar',
    mems ~ MemoryPlus ar ar') =>
    AsMemory ArrowPre a a ans fs ans' fs' '[Memory IO mems ()] where
        toProgram (Pre v) st inref = do
            (outref, st') <- fresh st
            let comp = readCells inref Rearrange.>>= writeCellsAfter outref
            writeRef outref v
            Prelude.return (st', comp :+: HNil, outref)

instance (AsMemory larr a b ans fs bns fs' progl, AsMemory rarr b c bns fs' cns fs'' progr,
    prog ~ Combine progl progr) =>
    AsMemory (ArrowGGG larr rarr b) a c ans fs cns fs'' prog where
        toProgram (f :>>>: g) st inref = do
            (st', compf, midref) <- toProgram f st inref
            (st'', compg, outref) <- toProgram g st' midref
            Prelude.return (st'', hCombine compf compg, outref)

instance (AsMemory larr la lb lans fs lbns fs' progl, AsMemory rarr ra rb rans fs' rbns fs'' progr,
    prog ~ Combine progl progr) =>
    AsMemory (ArrowSSS larr rarr) (P la ra) (P lb rb) (PN lans rans) fs (PN lbns rbns) fs'' prog where
        toProgram (f :***: g) st (PRef inl inr) = do
            (st', compf, outl) <- toProgram f st inl
            (st'', compg, outr) <- toProgram g st' inr
            Prelude.return (st'', hCombine compf compg, PRef outl outr)

-- TODO: We use two memory cells for loop constructs, which can definitely be optimised.

instance (Fresh c fs cns fs', AsMemory arr (P a c) (P b c) (PN ans cns) fs' (PN bns cns') fs'' inprog,
    CopyCells c cns' cns copy, prog ~ Combine inprog copy) =>
    AsMemory (ArrowLoop arr c) a b ans fs bns fs'' prog where
        toProgram (Loop f) st inref = do
            (loopin, st') <- fresh st
            (st'', compf, PRef out loopout) <- toProgram f st' (PRef inref loopin)
            let copyLoop = copyCells loopout loopin
            Prelude.return (st'', hCombine compf copyLoop, out)

data CompiledAFRP (env :: [*]) (prog :: [*]) (a :: Desc x) (b :: Desc y) ns ns' =
    CAFRP (Set env) (HList prog) (Ref ns a) (Ref ns' b)

-- The output cell of a pre will always contain the _next_ value, since it is written at the end of each run
-- through writeCellsAfter (this is needed to break the loop - we think of this as the pre preloading the next
-- value to return).
-- This means that if have the output cell of a pre as the output of the entire program, reading it will give
-- us the _next_ value being returned, not the current one.
-- We fix this by adding one separate output cell, which the current output is copied to before any call to
-- writeCellsAfter (just by using regular readCell >>= writeCell).
type AddSeparateOutput fs prog ans a fs' prog' ans' copy = (Fresh a fs ans' fs', CopyCells a ans ans' copy, prog' ~ Combine prog copy)
addSeparateOutput :: AddSeparateOutput fs prog ans a fs' prog' ans' copy
    => (BuildState fs, HList prog, Ref ans a) -> IO (BuildState fs', HList prog', Ref ans' a)
addSeparateOutput (st, prog, inref) = do
    (outref, st') <- fresh st
    let copyAns = copyCells inref outref
    Prelude.return (st', hCombine prog copyAns, outref)

makeAFRP :: (Fresh a '(0, '[]) ans fs, AsMemory arr a b ans fs bns fs' prog,
    AddSeparateOutput fs' prog bns b '(n', env') prog' bns' copy,
    Sortable env', Nubable (Sort env'))
    => AFRP arr a b -> IO (CompiledAFRP (AsSet env') prog' a b ans bns')
makeAFRP afrp = do
    (inref, st) <- fresh (MkBuildState HNil :: BuildState '(0, '[]))
    res <- toProgram afrp st inref
    (MkBuildState env, prog, outref) <- addSeparateOutput res
    Prelude.return $ CAFRP (hlistToSet env) prog inref outref

hlistToSet :: (Sortable xs, Nubable (Sort xs)) => HList xs -> Set (AsSet xs)
hlistToSet s = asSet (hls s)
    where
        hls :: HList xs -> Set xs
        hls HNil = Empty
        hls (x :+: xs) = Ext x (hls xs)

makeRunnable :: (MakeProgConstraints prog prog' env, RunMems_ IO prog' env) =>
    CompiledAFRP env prog a b ans bns -> IO (Val a -> IO (Val b))
makeRunnable (CAFRP env mems inref outref) = do
    prog <- makeProgram mems env
    Prelude.return $ \inp -> do
        writeRef inref inp
        runProgram_ prog
        readRef outref
