{-# LANGUAGE UndecidableInstances, QualifiedDo, ScopedTypeVariables #-}

module RAFRP (module AFRP, module GenProc.GeneralisedArrow, makeAFRP) where

import AFRP
import MakeMIO
import Naming
import Rearrange
import GenProc.GeneralisedArrow

import Data.Type.Set hiding (Proxy(..))
import Data.Proxy

makeAFRP :: forall a a' fs arr b arr' b' fs' b'' fs'' env prog env' env'' prog' prog''.
    (Fresh a EmptyFreshState a' fs,
    AssignMemory arr a b arr' a' b' fs fs',
    env ~ EnvFromBuildState fs',
    AsMemory arr' a' b' env prog,
    Augment prog b' fs' prog' b'' fs'',
    env' ~ EnvFromBuildState fs'',
    ProxToRef a' env', ProxToRef b'' env',
    Sortable env', Nubable (Sort env'),
    AsDesc a' ~ a, AsDesc b'' ~ b,
    env'' ~ AsSet env',
    MakeProgConstraints prog' prog'' env'', CompileMems_ prog'' env'') =>
    AFRP arr a b -> IO (Val a -> IO (Val b))
makeAFRP afrp = do
    (inprox, bs) <- fresh @_ @a newBuildState (Proxy :: Proxy a)
    (afrp', _, bs'@(MkBuildState env)) <- assignMemory afrp inprox bs
    (prog, outprox') <- toProgram afrp' (Proxy :: Proxy a') env
    (prog', outprox'', MkBuildState env') <- augment prog outprox' bs'
    let inref = proxToRef inprox env'
        outref = proxToRef outprox'' env'
    program <- makeProgram prog' (hlistToSet env')
    runner <- compileProgram_ program
    Prelude.return $ \inp -> do
        writeRef inref inp
        runner
        readRef outref

hlistToSet :: (Sortable xs, Nubable (Sort xs)) => HList xs -> Set (AsSet xs)
hlistToSet s = asSet (hls s)
    where
        hls :: HList xs -> Set xs
        hls HNil = Empty
        hls (x :+: xs) = Ext x (hls xs)