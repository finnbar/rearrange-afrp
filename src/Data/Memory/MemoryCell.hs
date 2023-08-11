-- This module defines the functions for reading and writing to Cells, and for
-- defining partial updates.

{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables,
    AllowAmbiguousTypes, FunctionalDependencies, FlexibleContexts #-}

module Data.Memory.MemoryCell (
    readCell, writeCell, readCellBefore,
    readIOCell, writeIOCell, readIOCellBefore,
    readAutoCell, writeAutoCell, readAutoCellBefore,
    Cell(..), CellUpdate(..),
    updated, updatedInEnv
    ) where

import Data.Memory.Types
import MonadRW

import GHC.TypeLits
import Data.Proxy
import Data.IORef

readCell :: forall s v t m. (MonadRW m v, Constr m v t) =>
    Memory m '( '[], '[], '[Cell v s t]) t
readCell = Mem $ \(Ext (Cell pt) Empty) -> readVar pt

writeCell :: forall s v t m. (MonadRW m v, Constr m v t) =>
    t -> Memory m '( '[], '[Cell v s t], '[] ) ()
writeCell x = Mem $ \(Ext (Cell pt) Empty) -> writeVar pt x

readCellBefore :: forall s v t m. (MonadRW m v, Constr m v t) =>
    Memory m '( '[Cell v s t], '[], '[]) t
readCellBefore = Mem $ \(Ext (Cell pt) Empty) -> readVar pt

readIOCell :: forall s t. MIO '( '[], '[], '[IOCell s t]) t
readIOCell = readCell

writeIOCell :: forall s t. t -> MIO '( '[], '[IOCell s t], '[]) ()
writeIOCell = writeCell

readIOCellBefore :: forall s t. MIO '( '[IOCell s t], '[], '[]) t
readIOCellBefore = readCellBefore

readAutoCell :: forall c s t. (c ~ AutoCell s t) =>
    Memory IO '( '[], '[], '[c]) t
readAutoCell = Mem $ \(Ext (AutoCell pt) Empty) -> readIORef pt

writeAutoCell :: forall c s t. (c ~ AutoCell s t) =>
    t -> Memory IO '( '[], '[c], '[]) ()
writeAutoCell x = Mem $ \(Ext (AutoCell pt) Empty) -> writeIORef pt x

readAutoCellBefore :: forall c s t. (c ~ AutoCell s t) =>
    Memory IO '( '[c], '[], '[]) t
readAutoCellBefore = Mem $ \(Ext (AutoCell pt) Empty) -> readIORef pt

updated :: forall s t v. KnownNat s => Cell v s t -> CellUpdate
updated _ = AddrUpdate $ show $ natVal (Proxy :: Proxy s)

updatedInEnv :: forall s env t v.
    (KnownNat s, MemberNat s env (Cell v s t)) => Set env -> CellUpdate
updatedInEnv = updated . memberNat (Proxy :: Proxy s)

class MemberNat s env out | s env -> out where
    memberNat :: Proxy s -> Set env -> out

instance (TypeError (Text "Cannot find " :<>: ShowType s :<>:
    Text " in environment for updatedInEnv."
    :$$: Text "Did you spell it correctly?"))
    => MemberNat s '[] () where
        memberNat _ _ = error "unreachable"

instance {-# OVERLAPPING #-} MemberNat s (Cell v s t ': xs) (Cell v s t) where
    memberNat _ (Ext addrS _) = addrS

instance {-# OVERLAPPABLE #-} MemberNat s xs o =>
    MemberNat s (Cell v s' t ': xs) o where
        memberNat prox (Ext _ mems) = memberNat prox mems