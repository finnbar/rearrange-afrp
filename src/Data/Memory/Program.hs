-- This module defines Program and ParallelProgram, which group memory
-- computations and the environment they run in, as well as resolving their
-- dependencies at type level.

{-# LANGUAGE FunctionalDependencies, RecordWildCards, FlexibleContexts #-}

module Data.Memory.Program (
    Program, makeProgram, runProgram, runProgram_,
    MakeProgConstraints, RunMems_, CompileMems_, compileProgram_
) where

import Data.Memory.Types (Set,  MIO , NoConflicts_) 
import Data.Memory.RunMemory (RunMems(..), RunMems_(..), CompileMems_(..))
import Data.Type.TSort (ordered, OrderedConstraints)
import Data.Type.HList (HList(..))
import Data.Memory.Dependencies (IsLessThan, NoOutputDep)

data Prog m e = Prog {
    mems :: HList m,
    env :: Set e
}

newtype Program m e = Program (Prog m e)

type MakeProgConstraints mems mems' env =
    (OrderedConstraints IsLessThan mems mems', NoConflicts_ env, NoOutputDep mems)

makeProgram :: (MakeProgConstraints mems mems' env) =>
    HList mems -> Set env -> IO (Program mems' env)
makeProgram mems env = do
    let mems' = ordered @IsLessThan mems
    return $ Program $ Prog {mems = mems', ..}

runProgram :: RunMems xs env out =>
    Program xs env -> IO (HList out)
runProgram (Program Prog {..}) = runMems mems env

runProgram_ :: RunMems_ xs env =>
    Program xs env -> IO ()
runProgram_ (Program Prog {..}) = runMems_ mems env

compileProgram_ :: CompileMems_ xs env =>
    Program xs env -> IO (IO ())
compileProgram_ (Program Prog {..}) = compileMems_ mems env
