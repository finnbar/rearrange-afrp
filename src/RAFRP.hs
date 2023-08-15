{-# LANGUAGE UndecidableInstances, QualifiedDo, FunctionalDependencies, ScopedTypeVariables #-}

module RAFRP (Ref(..), Desc(..), Val(..), RAFRP, CompiledRAFRP(..), writeRef, readRef,
    makeRAFRP, (>>>), (***), arr, loop, pre) where

import GHC.TypeLits
import Data.Type.HList

import Rearrange
import Data.Memory.Types (MemState, MemoryPlus, Set(..))
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
    V :: forall (a :: *). Nat -> a -> Desc O
    P :: Desc a -> Desc b -> Desc (a ::: b)

type Val :: forall s. Desc s -> *
data Val x where
    One :: !a -> Val (V sy a)
    Pair :: !(Val a) -> !(Val b) -> Val (P a b)

type Ref :: forall s. Desc s -> *
data Ref x where
    VRef :: IOCell n a -> Ref (V n a)
    PRef :: Ref l -> Ref r -> Ref (P l r)

class Fresh d (n :: Nat) (n' :: Nat) | d n -> n' where
    fresh :: MaxVar n -> IO (Ref d, MaxVar n')

instance (n' ~ n + 1) => Fresh (V n (a :: *)) n n' where
    fresh _ = do
        ir <- newIORef undefined
        Prelude.return (VRef $ Cell @n @a @IO ir, VarState)

instance (Fresh l n n', Fresh r n' n'') => Fresh (P l r) n n'' where
    fresh mv = do
        (l, mv') <- fresh mv
        (r, mv'') <- fresh mv'
        Prelude.return (PRef l r, mv'')

class Cells desc set | desc -> set where
    asCells :: Ref desc -> Set set

instance Cells (V n a) '[IOCell n a] where
    asCells (VRef i) = asSet $ Ext i Empty

instance (Cells l ls, Cells r rs, res ~ Union ls rs, Unionable ls rs) => Cells (P l r) res where
    asCells (PRef x y) = let
        setx = asCells @l @ls x
        sety = asCells @r @rs y
        in union setx sety :: Set res

readRef :: Ref d -> IO (Val d)
readRef (VRef (Cell i)) = One <$> readIORef i
readRef (PRef l r) = Pair <$> readRef l <*> readRef r

writeRef :: Ref d -> Val d -> IO ()
writeRef (VRef (Cell i)) (One v) = writeIORef i v
writeRef (PRef l r) (Pair i j) = writeRef l i Prelude.>> writeRef r j

type ReadCells :: forall (ar :: Arity). Desc ar -> MemState -> Constraint
class ReadCells (ac :: Desc ar) res | ac -> res where
    readCells :: Memory IO res (Val ac)

instance forall (s :: Nat) (a :: *). ReadCells (V s a) '( '[], '[], '[IOCell s a]) where
    readCells = One <$> readIOCell

instance (ReadCells lc lr, ReadCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => ReadCells (P lc rc) res where
    readCells = Rearrange.do
        left <- readCells
        Pair left <$> readCells

type ReadCellsBefore :: forall (ar :: Arity). Desc ar -> MemState -> Constraint
class ReadCellsBefore (ac :: Desc ar) res | ac -> res where
    readCellsBefore :: Memory IO res (Val ac)

instance forall (s :: Nat) (a :: *). ReadCellsBefore (V s a) '( '[IOCell s a], '[], '[]) where
    readCellsBefore = One <$> readIOCellBefore

instance (ReadCellsBefore lc lr, ReadCellsBefore rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => ReadCellsBefore (P lc rc) res where
    readCellsBefore = Rearrange.do
        left <- readCellsBefore
        Pair left <$> readCellsBefore

type WriteCells :: forall (ar :: Arity). Desc ar -> MemState -> Constraint
class WriteCells (ac :: Desc ar) res | ac -> res where
    writeCells :: Val ac -> Memory IO res ()

instance forall (s :: Nat) (a :: *). WriteCells (V s a) '( '[], '[IOCell s a], '[]) where
    writeCells (One v) = writeIOCell v

instance (WriteCells lc lr, WriteCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => WriteCells (P lc rc) res where
    writeCells (Pair a b) = Rearrange.do
        writeCells a
        writeCells b

type Equivalent :: forall (ar :: Arity). Desc ar -> Desc ar -> Constraint
class Equivalent d d' where
    equivalent :: Val d -> Val d'

instance Equivalent (V s a) (V s' a) where
    equivalent (One v) = One v

instance (Equivalent l l', Equivalent r r') => Equivalent (P l r) (P l' r') where
    equivalent (Pair l r) = Pair (equivalent l) (equivalent r)

-- In order to generate fresh type variables, we need a state to track which vars are unused.
-- Vars are Nat, so this stores the highest number used so far.
-- (The program is created by setting MaxVar to 0.)

data MaxVar (n :: Nat) = VarState
type RAFRP a b prog env (n :: Nat) (n' :: Nat) = MaxVar n -> IO (MaxVar n', Ref a -> (HList prog, Ref b, Set env))

arr :: forall a ar b br n n' seta setb.
    (ReadCells a ar, WriteCells b br, MemoryInv ar br, Fresh b n n', Cells a seta, Cells b setb, Unionable seta setb,
    seta ~ AsSet seta, setb ~ AsSet setb) =>
    (Val a -> Val b) -> RAFRP a b '[Memory IO (MemoryPlus ar br) ()] (Union seta setb) n n'
arr f mv = do
    let comp = (f <$> readCells) Rearrange.>>= writeCells
    (outref, mv') <- fresh mv
    Prelude.return (mv', \inref ->
        let incells :: Set seta
            incells = asCells @a @seta inref
            outcells :: Set setb
            outcells = asCells @b @setb outref
        in (comp :+: HNil, outref, incells `union` outcells))

pre :: forall (ari :: Arity) (a :: Desc ari) (a' :: Desc ari) ar ar' n n' seta seta'.
    (ReadCellsBefore a ar, WriteCells a' ar', MemoryInv ar ar', Equivalent a a', Cells a seta, Cells a' seta', Fresh a' n n',
    Unionable seta seta', seta ~ AsSet seta, seta' ~ AsSet seta') =>
    Val a -> RAFRP a a' '[Memory IO (MemoryPlus ar ar') ()] (Union seta seta') n n'
pre v mv = do
    let comp = (equivalent <$> readCellsBefore @_ @a @ar) Rearrange.>>= writeCells @_ @a' @ar'
    (outref, mv') <- fresh mv
    writeValToRef outref (equivalent v)
    Prelude.return (mv', \inref ->
        let incells :: Set seta
            incells = asCells inref
            outcells :: Set seta'
            outcells = asCells outref
        in (comp :+: HNil, outref, incells `union` outcells))
    where
        writeValToRef :: Ref d -> Val d -> IO ()
        writeValToRef (VRef (Cell i)) (One v) = writeIORef i v
        writeValToRef (PRef l r) (Pair lv rv) = writeValToRef l lv Prelude.>> writeValToRef r rv

(>>>) :: (Unionable env env') => RAFRP a b ks env n n' -> RAFRP b c ks' env' n' n'' -> RAFRP a c (Combine ks ks') (Union env env') n n''
f >>> g = \mv -> do
    (mv', f') <- f mv
    (mv'', g') <- g mv'
    Prelude.return (mv'', \inref ->
        let (prog, midref, env) = f' inref
            (prog', outref, env') = g' midref
        in (hCombine prog prog', outref, env `union` env'))

(***) :: (Unionable env env') => RAFRP a b ks env n n' -> RAFRP a' b' ks' env' n' n'' -> RAFRP (P a a') (P b b') (Combine ks ks') (Union env env') n n''
f *** g = \mv -> do
    (mv', f') <- f mv
    (mv'', g') <- g mv'
    Prelude.return (mv'', \(PRef l r) ->
        let (lprog, l', env) = f' l
            (rprog, r', env') = g' r
        in (hCombine lprog rprog, PRef l' r', env `union` env'))

loop :: forall a b c n n' n'' set prog env. (Fresh c n n', Cells c set, Unionable env set) => RAFRP (P a c) (P b c) prog env n' n'' -> RAFRP a b prog (Union env set) n n''
loop f mv = do
    (looped, mv') <- fresh mv
    (mv'', f') <- f mv'
    Prelude.return (mv'', \inref ->
        let (mems, PRef bp _, env) = f' (PRef inref looped)
            loopEnv :: Set set
            loopEnv = asCells @c looped
        in (mems, bp, env `union` loopEnv))

data CompiledRAFRP (env :: [*]) (prog :: [*]) (a :: Desc x) (b :: Desc y) = CRAFRP (Set env) (HList prog) (Ref a) (Ref b)

makeRAFRP :: (Fresh a 0 n) => RAFRP a b prog env n n' -> IO (CompiledRAFRP env prog a b)
makeRAFRP rafrp = do
    (inref, mv) <- fresh (VarState :: MaxVar 0)
    (VarState, compiled) <- rafrp mv
    let (prog, outref, env) = compiled inref
    Prelude.return $ CRAFRP env prog inref outref