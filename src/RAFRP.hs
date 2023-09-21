{-# LANGUAGE UndecidableInstances, QualifiedDo, FunctionalDependencies, ScopedTypeVariables #-}

module RAFRP (Ref(..), Desc(..), Val(..), AFRP(..), makeAFRP, makeRunnable) where

import GHC.TypeLits
import Data.Type.HList

import Rearrange
import Data.Memory.Types (MemState, MemoryPlus)
import Data.Memory.Memory (MemoryInv)
import Data.Type.Utils
import Data.Type.Set
import Data.Kind (Constraint)
import Data.IORef (newIORef, writeIORef, readIORef)

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

type Fresh :: forall s. Desc s -> Names s -> Nat -> Nat -> Constraint
class Fresh d ns (n :: Nat) (n' :: Nat) | d n -> ns n' where
    fresh :: MaxVar n -> IO (Ref ns d, MaxVar n')

instance (n' ~ n + 1) => Fresh (V (a :: *)) (VN n) n n' where
    fresh _ = do
        ir <- newIORef undefined
        Prelude.return (VRef $ Cell @n @a @IO ir, VarState)

instance (Fresh l ln n n', Fresh r rn n' n'') => Fresh (P l r) (PN ln rn) n n'' where
    fresh mv = do
        (l, mv') <- fresh mv
        (r, mv'') <- fresh mv'
        Prelude.return (PRef l r, mv'')

class Cells desc names set | desc names -> set where
    asCells :: Ref names desc -> Set set

instance Cells (V a) (VN n) '[IOCell n a] where
    asCells (VRef i) = asSet $ Ext i Empty

instance (Cells l ln ls, Cells r rn rs, res ~ Union ls rs, Unionable ls rs) => Cells (P l r) (PN ln rn) res where
    asCells (PRef x y) = let
        setx = asCells @l @ln x
        sety = asCells @r @rn y
        in union setx sety :: Set res

readRef :: Ref n d -> IO (Val d)
readRef (VRef (Cell i)) = One <$> readIORef i
readRef (PRef l r) = Pair <$> readRef l <*> readRef r

writeRef :: Ref n d -> Val d -> IO ()
writeRef (VRef (Cell i)) (One v) = writeIORef i v
writeRef (PRef l r) (Pair i j) = writeRef l i Prelude.>> writeRef r j

type ReadCells :: forall (ar :: Arity). Desc ar -> Names ar -> MemState -> Constraint
class ReadCells ac ns res | ac ns -> res where
    readCells :: Ref ns ac -> Memory IO res (Val ac)

instance ReadCells (V a) (VN n) '( '[], '[], '[IOCell n a]) where
    readCells _ = One <$> readIOCell

instance (ReadCells lc ln lr, ReadCells rc rn rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => ReadCells (P lc rc) (PN ln rn) res where
    readCells (PRef lp rp) = Rearrange.do
        left <- readCells lp
        Pair left <$> readCells rp

type ReadCellsBefore :: forall (ar :: Arity). Desc ar -> Names ar -> MemState -> Constraint
class ReadCellsBefore ac ns res | ac ns -> res where
    readCellsBefore :: Ref ns ac -> Memory IO res (Val ac)

instance ReadCellsBefore (V a) (VN n) '( '[IOCell n a], '[], '[]) where
    readCellsBefore _ = One <$> readIOCellBefore

instance (ReadCellsBefore lc ln lr, ReadCellsBefore rc rn rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => ReadCellsBefore (P lc rc) (PN ln rn) res where
    readCellsBefore (PRef lp rp) = Rearrange.do
        left <- readCellsBefore lp
        Pair left <$> readCellsBefore rp

type WriteCells :: forall (ar :: Arity). Desc ar -> Names ar -> MemState -> Constraint
class WriteCells ac ns res | ac ns -> res where
    writeCells :: Ref ns ac -> Val ac -> Memory IO res ()

instance WriteCells (V a) (VN n) '( '[], '[IOCell n a], '[]) where
    writeCells _ (One v) = writeIOCell v

instance (WriteCells lc ln lr, WriteCells rc rn rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => WriteCells (P lc rc) (PN ln rn) res where
    writeCells (PRef lp rp) (Pair a b) = Rearrange.do
        writeCells lp a
        writeCells rp b

type UnifyCells :: forall (ar :: Arity). Desc ar -> Names ar -> Names ar -> [*] -> Constraint
class UnifyCells d ns ns' prog | d ns ns' -> prog where
    unifyCells :: Ref ns d -> Ref ns' d -> HList prog

instance (ReadCells (V a) (VN n) resl, WriteCells (V a) (VN n') resr, res ~ MemoryPlus resl resr, MemoryInv resl resr)
    => UnifyCells (V a) (VN n) (VN n') '[Memory IO res ()] where
        unifyCells ra ra' =
            let comp = readCells ra Rearrange.>>= writeCells ra'
            in comp :+: HNil

instance (UnifyCells l ln ln' resl, UnifyCells r rn rn' resr, res ~ Combine resl resr)
    => UnifyCells (P l r) (PN ln rn) (PN ln' rn') res where
        unifyCells (PRef ra ra') (PRef rb rb') =
            let compl = unifyCells ra rb
                compr = unifyCells ra' rb'
            in hCombine compl compr

-- Programmers write their code in this intermediate GADT.
-- This is so that programmers can write programs without worrying about names.
-- This should allow full inference: assume we have a well-typed AFRP, then we transform to well-typed RAFRP,
-- which can then be (possibly) implemented with rearrange.

type Arrow :: Arity -> Arity -> *
data Arrow ar ar' where
    ArrowId :: Arrow a a
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
    Id :: AFRP ArrowId a a
    Arr :: (Val a -> Val b) -> AFRP ArrowArr a b
    Pre :: Val a -> AFRP ArrowPre a a
    (:>>>:) :: AFRP ar a b -> AFRP ar' b c -> AFRP (ArrowGGG ar ar' b) a c
    (:***:) :: AFRP ar a b -> AFRP ar' a' b' -> AFRP (ArrowSSS ar ar') (P a a') (P b b')
    Loop :: AFRP ar (P a c) (P b c) -> AFRP (ArrowLoop ar c) a b

-- In order to generate fresh type variables, we need a state to track which vars are unused.
-- Vars are Nat, so this stores the highest number used so far.
-- (The program is created by setting MaxVar to 0.)

data MaxVar (n :: Nat) = VarState

-- Since the type of the Memory computation as output is going to differ based on which
-- Arrow combinator we use, we need a type family to guide the relevant type inference.
-- Given the arrow combinator, its input and output types, its input name and MaxVar,
-- we need the output name, MaxVar and the resulting program.

type AsMemory :: forall (ar :: Arity) (br :: Arity). Arrow ar br -> Desc ar -> Desc br -> Names ar -> Nat -> Names br -> Nat -> [*] -> [*] -> Constraint
class AsMemory arr a b ns n ns' n' prog env | arr a b ns n -> ns' n' prog env where
    toProgram :: AFRP arr a b -> MaxVar n -> Ref ns a -> IO (MaxVar n', HList prog, Ref ns' b, Set env)

instance AsMemory ArrowId a a ns n ns n '[] '[] where
    toProgram Id mv rf = Prelude.return (mv, HNil, rf, Empty)

instance (Fresh b ns' n n', ReadCells a ns ar, WriteCells b ns' br,
    Cells a ns seta, Cells b ns' setb, MemoryInv ar br, Unionable seta setb,
    prog ~ '[Memory IO (MemoryPlus ar br) ()],
    env ~ Union seta setb) =>
    AsMemory ArrowArr a b ns n ns' n' prog env where
        toProgram (Arr f) mv inref = do
            (outref, mv') <- fresh mv
            let comp = (f <$> readCells inref) Rearrange.>>= writeCells outref
                incells :: Set seta
                incells = asCells @a @ns inref
                outcells :: Set setb
                outcells = asCells @b @ns' outref
            Prelude.return (mv', comp :+: HNil, outref, incells `union` outcells)

instance (Fresh a ns' n n', ReadCellsBefore a ns ar, WriteCells a ns' ar', MemoryInv ar ar',
    Cells a ns seta, Cells a ns' seta', Unionable seta seta',
    prog ~ '[Memory IO (MemoryPlus ar ar') ()],
    env ~ Union seta seta') =>
    AsMemory ArrowPre a a ns n ns' n' prog env where
        toProgram (Pre v) mv inref = do
            (outref, mv') <- fresh mv
            let comp = readCellsBefore inref Rearrange.>>= writeCells outref
                incells :: Set seta
                incells = asCells inref
                outcells :: Set seta'
                outcells = asCells outref
            writeRef inref v
            Prelude.return (mv', comp :+: HNil, outref, incells `union` outcells)

instance (AsMemory l a b ns n ns' n' progl envl, AsMemory r b c ns' n' ns'' n'' progr envr,
    prog ~ Combine progl progr, env ~ Union envl envr, Unionable envl envr) =>
    AsMemory (ArrowGGG l r b) a c ns n ns'' n'' prog env where
        toProgram (f :>>>: g) mv inref = do
            (mv', compf, midref, envl) <- toProgram f mv inref
            (mv'', compg, outref, envr) <- toProgram g mv' midref
            Prelude.return (mv'', hCombine compf compg, outref, envl `union` envr)

instance (AsMemory l la lb lns n lns' n' progl envl, AsMemory r ra rb rns n' rns' n'' progr envr,
    prog ~ Combine progl progr, env ~ Union envl envr, Unionable envl envr) =>
    AsMemory (ArrowSSS l r) (P la ra) (P lb rb) (PN lns rns) n (PN lns' rns') n'' prog env where
        toProgram (f :***: g) mv (PRef inl inr) = do
            (mv', compf, outl, envl) <- toProgram f mv inl
            (mv'', compg, outr, envr) <- toProgram g mv' inr
            Prelude.return (mv'', hCombine compf compg, PRef outl outr, envl `union` envr)

-- TODO: We use two memory cells for loop constructs, which can definitely be optimised.

instance (Fresh c cns n n', AsMemory arr (P a c) (P b c) (PN ns cns) n' (PN ns'' cns') n'' inprog inenv,
    env ~ Union inenv setcs, setcs ~ Union setc setc', Cells c cns setc, Cells c cns' setc',
    Unionable inenv setcs, Unionable setc setc', UnifyCells c cns' cns unify, prog ~ Combine inprog unify) =>
    AsMemory (ArrowLoop arr c) a b ns n ns'' n'' prog env where
        toProgram (Loop f) mv inref = do
            (loopin, mv') <- fresh mv
            (mv'', compf, PRef out loopout, env) <- toProgram f mv' (PRef inref loopin)
            let 
                loopinEnv = asCells loopin
                loopOutEnv = asCells loopout
                unifyLoop = unifyCells loopout loopin
            Prelude.return (mv'', hCombine compf unifyLoop, out, env `union` (loopinEnv `union` loopOutEnv))

data CompiledAFRP (env :: [*]) (prog :: [*]) (a :: Desc x) (b :: Desc y) ns ns' =
    CAFRP (Set env) (HList prog) (Ref ns a) (Ref ns' b)

makeAFRP :: (Fresh a ns 0 n, AsMemory arr a b ns n ns' n' prog env) 
    => AFRP arr a b -> IO (CompiledAFRP env prog a b ns ns')
makeAFRP afrp = do
    (inref, mv) <- fresh (VarState :: MaxVar 0)
    (VarState, prog, outref, env) <- toProgram afrp mv inref
    Prelude.return $ CAFRP env prog inref outref

makeRunnable :: (MakeProgConstraints prog prog' env, RunMems_ IO prog' env) =>
    CompiledAFRP env prog a b ns ns' -> IO (Val a -> IO (Val b))
makeRunnable (CAFRP env mems inref outref) = do
    prog <- makeProgram mems env
    Prelude.return $ \inp -> do
        writeRef inref inp
        runProgram_ prog
        readRef outref
