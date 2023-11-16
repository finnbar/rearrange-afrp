{-# LANGUAGE UndecidableInstances, QualifiedDo, ScopedTypeVariables,
    FunctionalDependencies #-}

module RAFRP (module AFRP, makeAFRP, makeRunnable) where

import AFRP
import MakeMIO
import Naming
import Rearrange

import Data.Type.Set hiding (Proxy(..))
import Data.Proxy

-- The output cell of a pre will always contain the _next_ value, since it is written at the end of each run
-- through writeCellsAfter (this is needed to break the loop - we think of this as the pre preloading the next
-- value to return).
-- This means that if have the output cell of a pre as the output of the entire program, reading it will give
-- us the _next_ value being returned, not the current one.
-- We fix this by adding one separate output cell by postcomposing >>> arr id to the input.
-- TODO I thought I could be smart with >>> arr id, but it makes Haskell panic.
-- Make the Augment type class, which adds on the final memory cell.
-- NOTE The tests in Main currently work.
makeAFRP :: forall a a' fs arr b arr' b' fs' env prog.
    (Fresh a EmptyFreshState a' fs,
    AssignMemory arr a b arr' a' b' fs fs',
    env ~ EnvFromBuildState fs',
    AsMemory arr' a' b' env prog,
    ProxToRef a' env, ProxToRef b' env,
    Sortable env, Nubable (Sort env)) =>
    AFRP arr a b -> IO (CompiledAFRP (AsSet env) prog a' b')
makeAFRP afrp = do
    (inprox, bs) <- fresh @_ @a newBuildState (Proxy :: Proxy a)
    (afrp', _, MkBuildState env) <- assignMemory afrp inprox bs
    (prog, outprox') <- toProgram afrp' (Proxy :: Proxy a') env
    let inref = proxToRef inprox env
        outref = proxToRef outprox' env
    Prelude.return $ CAFRP (hlistToSet env) prog inref outref

hlistToSet :: (Sortable xs, Nubable (Sort xs)) => HList xs -> Set (AsSet xs)
hlistToSet s = asSet (hls s)
    where
        hls :: HList xs -> Set xs
        hls HNil = Empty
        hls (x :+: xs) = Ext x (hls xs)

makeRunnable :: (MakeProgConstraints prog prog' env, RunMems_ IO prog' env) =>
    CompiledAFRP env prog a b -> IO (Val (AsDesc a) -> IO (Val (AsDesc b)))
makeRunnable (CAFRP env mems inref outref) = do
    prog <- makeProgram mems env
    Prelude.return $ \inp -> do
        writeRef inref inp
        runProgram_ prog
        readRef outref