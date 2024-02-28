-- A simple module reexporting everything for end-user use.
module Rearrange (
    readCell, writeCell, writeCellAfter, memoryIO, unsafeMemoryIO, MemAft(..), Cell(..),
    hCombine, ifThenElse, HList(..), Effect(..), ordered,
    toSortedComponents, module DMP) where

import Data.Memory.Memory (memoryIO, unsafeMemoryIO, ifThenElse)
import Data.Memory.MemoryCell (readCell, writeCell, writeCellAfter)
import Data.Memory.Types (MemAft(..), Cell(..))
import Control.Effect (Effect(..))
import Data.Type.HList (HList(..), hCombine)
import Data.Type.TSort (ordered)
import Data.Type.ComponentSearch (toSortedComponents)
import Data.Memory.Program as DMP