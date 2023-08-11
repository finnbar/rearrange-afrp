{-# LANGUAGE UndecidableInstances, QualifiedDo, FunctionalDependencies, ScopedTypeVariables, AllowAmbiguousTypes #-}

module RAFRP where

import GHC.TypeLits
import Data.Type.HList

import Rearrange
import Data.Memory.Types (MemState, MemoryPlus, Set(..))
import Data.Memory.Memory (MemoryInv)
import Data.Type.Utils
import Data.Kind (Constraint)

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

type DProx :: forall s. Desc s -> *
data DProx x where
    VProx :: DProx (V s a)
    PProx :: DProx l -> DProx r -> DProx (P l r)

class MakeProx d where
    makeProx :: DProx d

instance MakeProx (V s a) where
    makeProx = VProx

instance (MakeProx l, MakeProx r) => MakeProx (P l r) where
    makeProx = PProx makeProx makeProx

type ReadCells :: forall (ar :: Arity). Desc ar -> MemState -> Constraint
class ReadCells (ac :: Desc ar) res | ac -> res where
    readCells :: Memory IO res (Val ac)

instance forall (s :: Nat) (a :: *). ReadCells (V s a) '( '[], '[], '[AutoCell s a]) where
    readCells = One <$> readAutoCell

instance (ReadCells lc lr, ReadCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => ReadCells (P lc rc) res where
    readCells = Rearrange.do
        left <- readCells
        Pair left <$> readCells

type ReadCellsBefore :: forall (ar :: Arity). Desc ar -> MemState -> Constraint
class ReadCellsBefore (ac :: Desc ar) res | ac -> res where
    readCellsBefore :: Memory IO res (Val ac)

instance forall (s :: Nat) (a :: *). ReadCellsBefore (V s a) '( '[AutoCell s a], '[], '[]) where
    readCellsBefore = One <$> readAutoCellBefore

instance (ReadCellsBefore lc lr, ReadCellsBefore rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => ReadCellsBefore (P lc rc) res where
    readCellsBefore = Rearrange.do
        left <- readCellsBefore
        Pair left <$> readCellsBefore

type WriteCells :: forall (ar :: Arity). Desc ar -> MemState -> Constraint
class WriteCells (ac :: Desc ar) res | ac -> res where
    writeCells :: Val ac -> Memory IO res ()

instance forall (s :: Nat) (a :: *). WriteCells (V s a) '( '[], '[AutoCell s a], '[]) where
    writeCells (One v) = writeAutoCell v

instance (WriteCells lc lr, WriteCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr) => WriteCells (P lc rc) res where
    writeCells (Pair a b) = Rearrange.do
        writeCells a
        writeCells b

type Equivalent :: forall (ar :: Arity). Desc ar -> Desc ar -> Constraint
class Equivalent d d' where
    equivalent :: Val d -> Val d'
    equivalentProx :: DProx d -> DProx d'

instance Equivalent (V s a) (V s' a) where
    equivalent (One v) = One v
    equivalentProx VProx = VProx

instance (Equivalent l l', Equivalent r r') => Equivalent (P l r) (P l' r') where
    equivalent (Pair l r) = Pair (equivalent l) (equivalent r)
    equivalentProx (PProx l r) = PProx (equivalentProx l) (equivalentProx r)

-- In order to generate fresh type variables, we need a state to track which vars are unused.
-- Vars are Nat, so this stores the highest number used so far.
-- (The program is created by setting MaxVar to 0.)

data MaxVar (n :: Nat) = VarState

type FreshVars :: forall d. Desc d -> Nat -> (Constraint, Nat)
type family FreshVars desc vs where
    FreshVars (V s a) n = '((n + 1) ~ s, n + 1)
    FreshVars (P l r) n = FreshVars' r (FreshVars l n)

type FreshVars' :: forall d. Desc d -> (Constraint, Nat) -> (Constraint, Nat)
type family FreshVars' desc state where
    FreshVars' d '(constr, n) = FreshVars'' constr (FreshVars d n)

type FreshVars'' :: Constraint -> (Constraint, Nat) -> (Constraint, Nat)
type family FreshVars'' constr state where
    FreshVars'' constr '(constr', n) = '((constr, constr'), n)

-- Arr f produces a single operation consisting of: read its inputs, do f, write its outputs.
-- Pre v produces a pair of operations - preread to get the output, then write to update the cell.
-- Combinators take the union of sets of Memory computations.
-- RAFRP is likely then just a set of Memory computations, and thus that's what is outputted on using a combinator.

-- TODO We're gonna have to make the cells manually. Fun.
-- This does make FreshVars less fragile though, as we'll get value-level vars as well as type-level ones.
-- This also means no need for HList startup (as we just correctly instantiate the cells) and its surrounding stuff.

type RAFRP a b startup repeated (n :: Nat) (n' :: Nat) = (DProx a, MaxVar n) -> (HList startup, HList repeated, DProx b, MaxVar n')

arr :: (ReadCells a ar, WriteCells b br, MemoryInv ar br, MakeProx b, FreshVars b n ~ '(constr, n'), constr) =>
    (Val a -> Val b) -> RAFRP a b '[] '[Memory IO (MemoryPlus ar br) ()] n n'
arr f (_, vs) = let
        comp = (f <$> readCells) Rearrange.>>= writeCells
    in (HNil, comp :+: HNil, makeProx, VarState)

pre :: forall (ari :: Arity) (a :: Desc ari) (a' :: Desc ari) ar ar' startup n n' constr.
    (ReadCellsBefore a ar, WriteCells a' ar', MemoryInv ar ar', WriteCells a startup, Equivalent a a', FreshVars a' n ~ '(constr, n'), constr) =>
    Val a -> RAFRP a a' '[Memory IO startup ()] '[Memory IO (MemoryPlus ar ar') ()] n n'
pre v (prox, _) = let
        comp = (equivalent <$> readCellsBefore @_ @a @ar) Rearrange.>>= writeCells @_ @a' @ar'
    in (writeCells v :+: HNil, comp :+: HNil, equivalentProx prox, VarState)

(>>>) :: RAFRP a b ss ks n n' -> RAFRP b c ss' ks' n' n'' -> RAFRP a c (Combine ss ss') (Combine ks ks') n n''
f >>> g = \(prox, mv) -> let
        (starts, mems, prox', mv') = f (prox, mv)
        (starts', mems', prox'', mv'') = g (prox', mv')
    in (hCombine starts starts', hCombine mems mems', prox'', mv'')

(***) :: RAFRP a b ss ks n n' -> RAFRP a' b' ss' ks' n' n'' -> RAFRP (P a a') (P b b') (Combine ss ss') (Combine ks ks') n n''
f *** g = \(PProx l r, mv) -> let
        (lstart, lmem, l', mv') = f (l, mv)
        (rstart, rmem, r', mv'') = g (r, mv')
    in (hCombine lstart rstart, hCombine lmem rmem, PProx l' r', mv'')

loop :: MakeProx c => RAFRP (P a c) (P b c) ss ks n n' -> RAFRP a b ss ks n n'
loop f (prox, mv) = let
        (starts, mems, PProx bp _, mv') = f (PProx prox makeProx, mv)
    in (starts, mems, bp, mv')

-- IDEA: To avoid constraints hell, just write a function `RAFRP -> Env -> HList` that can be run.
-- Then programmers can `makeProgram` etc and not have any issues.