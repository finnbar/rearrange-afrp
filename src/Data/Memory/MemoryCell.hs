-- This module defines the functions for reading and writing to Cells, and for
-- defining partial updates.

{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables,
    AllowAmbiguousTypes, FlexibleContexts #-}

module Data.Memory.MemoryCell (
    readCell, writeCell, writeCellAfter, Cell(..)) where

import Data.Memory.Types

import Data.IORef

readCell :: forall s t. 
    MemAft '( '[], '[Cell s t], '[]) t
readCell = Mem $ \(Ext (Cell pt) Empty) -> readIORef pt

writeCell :: forall s t.
    t -> MemAft '( '[Cell s t], '[], '[] ) ()
writeCell x = Mem $ \(Ext (Cell pt) Empty) -> writeIORef pt x

writeCellAfter :: forall s t.
    t -> MemAft '( '[], '[], '[Cell s t] ) ()
writeCellAfter x = Mem $ \(Ext (Cell pt) Empty) -> writeIORef pt x