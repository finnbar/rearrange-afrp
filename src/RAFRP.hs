{-# LANGUAGE UndecidableInstances, QualifiedDo, ScopedTypeVariables, Strict #-}

module RAFRP (module AFRP, makeAFRP, toRearrangeable) where

import AFRP
import MakeMIO
import Naming
import Rearrange
import AFRP
import Data.Type.HList

import Data.Type.Set hiding (Proxy(..))
import Data.Proxy

-- TODO rename all arr to con, global type var renaming
makeAFRP :: forall a a' fs arr b arr' b' fs' b'' fs'' env prog env' env'' prog' prog''.
    (Fresh a EmptyFreshKind a' fs,
    AssignMemory arr a b arr' a' b' fs fs',
    ToMIO arr' a' b' prog,
    Augment prog b' fs' prog' b'' fs'',
    env ~ EnvFromFreshState fs'',
    HReverse env env',
    ProxToRef a' env', ProxToRef b'' env',
    Sortable env', Nubable (Sort env'),
    AsDesc a' ~ a, AsDesc b'' ~ b,
    env'' ~ AsSet env',
    MakeProgConstraints prog' prog'' env'', CompileMems_ prog'' env'') =>
    SF arr a b -> IO (Val a -> IO (Val b))
makeAFRP afrp = do
    (inprox, bs) <- fresh @_ @a newFreshState (Proxy :: Proxy a)
    (afrp', _, bs') <- assignMemory afrp inprox bs
    (prog, outprox') <- toMIO afrp' (Proxy :: Proxy a')
    (prog', outprox'', MkFreshState env) <- augment prog outprox' bs'
    let env' = hReverse env
        inref = proxToRef inprox env'
        outref = proxToRef outprox'' env'
    program <- makeProgram prog' (hlistToSet env')
    runner <- compileProgram_ program
    Prelude.return $ \inp -> do
        writeRef inref inp
        runner
        readRef outref

-- This bypasses all of the Rearrange stuff, which we need in order to see if it compiles.
toRearrangeable :: forall a a' fs arr b arr' b' fs' b'' fs'' env prog env' prog'.
    (Fresh a EmptyFreshKind a' fs,
    AssignMemory arr a b arr' a' b' fs fs',
    ToMIO arr' a' b' prog,
    Augment prog b' fs' prog' b'' fs'',
    env ~ EnvFromFreshState fs'',
    HReverse env env',
    AsDesc a' ~ a, AsDesc b'' ~ b,
    ProxToRef a' env', ProxToRef b'' env') =>
    SF arr a b -> IO (HList prog', HList env', Ref a', Ref b'')
toRearrangeable afrp = do
    (inprox, bs) <- fresh @_ @a newFreshState (Proxy :: Proxy a)
    (afrp', _, bs') <- assignMemory afrp inprox bs
    (prog, outprox') <- toMIO afrp' (Proxy :: Proxy a')
    (prog', outprox'', MkFreshState env) <- augment prog outprox' bs'
    -- The env must be reversed here because it is constructed in reverse - Fresh prepends
    -- rather than appends.
    let env' = hReverse env
        inref = proxToRef inprox env'
        outref = proxToRef outprox'' env'
    Prelude.return (prog', env', inref, outref)

hlistToSet :: (Sortable xs, Nubable (Sort xs)) => HList xs -> Set (AsSet xs)
hlistToSet s = asSet (hls s)
    where
        hls :: HList xs -> Set xs
        hls HNil = Empty
        hls (x :+: xs) = Ext x (hls xs)