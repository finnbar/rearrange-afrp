-- This module defines all of the relevant types for working with memory, and
-- reexports part of Data.Type.Set.

{-# LANGUAGE ExplicitForAll, FlexibleInstances, FlexibleContexts,
    UndecidableInstances, AllowAmbiguousTypes #-}

module Data.Memory.Types (
    Memory(..), Cell(..), CellUpdate(..), IsMemory,
    Split(..), Set(..), Sort, MemoryUnion, MemoryPlus, MemoryWrites,
    MemoryReads, MemoryPostWrites, MIO, IOCell, TrioUnion, MemState,
    Subset(..), NoConflicts, NoConflicts_, AutoCell(..), GetName,
) where

import MonadRW (MonadRW(..))

import Data.Type.Set
import GHC.TypeLits (Nat, CmpNat, TypeError, ErrorMessage(..))
import Data.Kind (Constraint)
import Data.IORef (IORef)

type MemState = ([*], [*], [*])

-- NOTE: This version of memory now has three sections: writes, reads and postwrites.
-- They execute in that order.
newtype Memory (m :: * -> *) (s :: MemState) a =
    Mem { runMemory :: Set (MemoryUnion s) -> m a }

instance Functor m => Functor (Memory m s) where
    fmap f (Mem rm) = Mem $ \s -> f <$> rm s

type family MemoryPostWrites x :: [*] where
    MemoryPostWrites (Memory m '(ws, rs, ps) a) = ps

type family MemoryWrites x :: [*] where
    MemoryWrites (Memory m '(ws, rs, ps) a) = ws

type family MemoryReads x :: [*] where
    MemoryReads (Memory m '(ws, rs, ps) a) = rs

data Cell (v :: * -> *) (s :: Nat) (t :: *) where
    Cell :: forall s t m v. (Monad m, MonadRW m v, Constr m v t) => v t -> Cell v s t

data AutoCell (s :: Nat) (t :: *) where
    AutoCell :: forall s t. IORef t -> AutoCell s t

newtype CellUpdate = AddrUpdate String

type MIO s a = Memory IO s a
type IOCell s t = Cell IORef s t

type instance Cmp (Cell v s t) (Cell v' s' t') = CmpNat s s'
type instance Cmp (AutoCell s t) (AutoCell s' t') = CmpNat s s'
type instance Cmp (Cell v s t) (AutoCell s' t') = CmpNat s s'
type instance Cmp (AutoCell s t) (Cell v s' t') = CmpNat s s'

-- Specific definitions of Union and IsSet for lists of Cells.

-- Definitions of Union and IsSet for sets of Memory, which are just
-- elementwise lists of Cells.

type TrioUnion as bs cs = Union as (Union bs cs)

type family MemoryUnion (s :: MemState) :: [*] where
    MemoryUnion '(ws, rs, ps) = TrioUnion ws rs ps

type family MemoryPlus (s :: MemState) (t :: MemState) :: MemState where
    MemoryPlus '(ws, rs, ps) '(ws', rs', ps') = '(Union ws ws', Union rs rs', Union ps ps')

type family IsMemory (x :: MemState) :: Constraint where
    IsMemory '(s, t, u) = (IsSet s, IsSet t, IsSet u)

type family GetName (x :: *) :: Nat where
    GetName (Cell v s t) = s
    GetName (AutoCell s t) = s
    GetName x = TypeError (Text "Cannot get the name of " :<>: ShowType x
        :$$: Text "It must be a Cell or AutoCell!")

type family IfSameName (x :: Nat) (y :: Nat) (tru :: Constraint)
        (fals :: Constraint) :: Constraint where
    IfSameName x x tru fals = tru
    IfSameName x y tru fals = fals

type family NoConflicts_ (t :: [*]) :: Constraint where
    NoConflicts_ '[] = ()
    NoConflicts_ '[x] = ()
    NoConflicts_ (x ': (x ': e)) =
        NoConflicts_ (x ': e)
    NoConflicts_ (x ': (y ': e)) =
        IfSameName (GetName x) (GetName y)
            (TypeError (Text "Names must uniquely define cells!"
            :$$: ShowType x :<>: Text " and " :<>: ShowType y :<>: Text " fail this!"))
            (NoConflicts_ (y ': e))

type family NoConflicts (x :: MemState) :: Constraint where
    NoConflicts '(s, t, u) = NoConflicts_ (TrioUnion s t u)