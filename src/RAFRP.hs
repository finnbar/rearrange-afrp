{-# LANGUAGE UndecidableInstances, QualifiedDo, ScopedTypeVariables #-}

module RAFRP (module AFRP, makeAFRP, makeRunnable) where

import AFRP
import MakeMIO
import Naming
import Rearrange

import Data.Type.Set hiding (Proxy(..))
import Data.Proxy

data CompiledAFRP (env :: [*]) (prog :: [*]) (a :: Desc' x) (b :: Desc' y) =
    CAFRP (Set env) (HList prog) (Ref a) (Ref b)

makeAFRP :: forall a a' fs arr b arr' b' fs' b'' fs'' env prog env' prog'.
    (Fresh a EmptyFreshState a' fs,
    AssignMemory arr a b arr' a' b' fs fs',
    env ~ EnvFromBuildState fs',
    AsMemory arr' a' b' env prog,
    Augment prog b' fs' prog' b'' fs'',
    env' ~ EnvFromBuildState fs'',
    ProxToRef a' env', ProxToRef b'' env',
    Sortable env', Nubable (Sort env')) =>
    AFRP arr a b -> IO (CompiledAFRP (AsSet env') prog' a' b'')
makeAFRP afrp = do
    (inprox, bs) <- fresh @_ @a newBuildState (Proxy :: Proxy a)
    (afrp', _, bs'@(MkBuildState env)) <- assignMemory afrp inprox bs
    (prog, outprox') <- toProgram afrp' (Proxy :: Proxy a') env
    (prog', outprox'', MkBuildState env') <- augment prog outprox' bs'
    let inref = proxToRef inprox env'
        outref = proxToRef outprox'' env'
    Prelude.return $ CAFRP (hlistToSet env') prog' inref outref

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