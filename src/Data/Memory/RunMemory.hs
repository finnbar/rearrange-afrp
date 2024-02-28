-- This module runs groups of computations serially, and also does partial
-- updates for serial groups of computations.

{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances,
    ScopedTypeVariables, FunctionalDependencies #-}

module Data.Memory.RunMemory (
    runMem, RunMems(..), runMem_, RunMems_(..), CompileMems_(..)
) where

import Data.Memory.Types

import Data.Type.HList

runMem :: Subset (MemoryUnion s) env => MemAft s b -> Set env -> IO b
runMem mem env = runMemory mem (subset env)

class RunMems xs env out | xs env -> out where
    runMems :: HList xs -> Set env -> IO (HList out)

instance RunMems '[] env '[] where
    runMems _ _ = return HNil

instance (RunMems xs env out, Subset (MemoryUnion s) env) =>
    RunMems (MemAft s b ': xs) env (b ': out) where
        runMems (mem :+: mems) env = do
            r <- runMem mem env
            rs <- runMems mems env
            return $ r :+: rs

runMem_ :: Subset (MemoryUnion s) env => MemAft s () -> Set env -> IO ()
runMem_ mem env = runMemory mem (subset env)

class RunMems_ xs env where
    runMems_ :: HList xs -> Set env -> IO ()

instance RunMems_ '[] env where
    runMems_ _ _ = return ()

instance (RunMems_ xs env, Subset (MemoryUnion s) env) =>
    RunMems_ (MemAft s () ': xs) env where
        runMems_ (mem :+: mems) env = runMem_ mem env >> runMems_ mems env

class CompileMems_ xs env where
    compileMems_ :: HList xs -> Set env -> IO (IO ())

instance CompileMems_ '[] env where
    compileMems_ _ _ = return (return ())

instance (CompileMems_ xs env, Subset (MemoryUnion s) env) =>
    CompileMems_ (MemAft s () ': xs) env where
        compileMems_ (mem :+: mems) env = do
            comps <- compileMems_ mems env
            return (runMem_ mem env >> comps)