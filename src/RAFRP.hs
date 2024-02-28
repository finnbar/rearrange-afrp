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

makeAFRP :: forall con inp out fk0 conAnn inpAnn outAnn fk1 prog prog' outAug fk2 env env' env'' prog''.
    (Fresh inp EmptyFreshKind inpAnn fk0,
    AssignMemory con inp out conAnn inpAnn outAnn fk0 fk1,
    ToMIO conAnn inpAnn outAnn prog,
    Augment prog outAnn fk1 prog' outAug fk2,
    env ~ EnvFromFreshState fk2,
    HReverse env env',
    ProxToRef inpAnn env', ProxToRef outAug env',
    Sortable env', Nubable (Sort env'),
    AsDesc inpAnn ~ inp, AsDesc outAug ~ out,
    env'' ~ AsSet env',
    MakeProgConstraints prog' prog'' env'', CompileMems_ prog'' env'') =>
    SF con inp out -> IO (Val inp -> IO (Val out))
makeAFRP afrp = do
    (inprox, bs) <- fresh @_ @inp newFreshState (Proxy :: Proxy inp)
    (afrp', _, bs') <- assignMemory afrp inprox bs
    (prog, outprox') <- toMIO afrp' (Proxy :: Proxy inpAnn)
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
toRearrangeable :: forall con inp out fk0 conAnn inpAnn outAnn fk1 prog prog' outAug fk2 env env'.
    (Fresh inp EmptyFreshKind inpAnn fk0,
    AssignMemory con inp out conAnn inpAnn outAnn fk0 fk1,
    ToMIO conAnn inpAnn outAnn prog,
    Augment prog outAnn fk1 prog' outAug fk2,
    env ~ EnvFromFreshState fk2,
    HReverse env env',
    AsDesc inpAnn ~ inp, AsDesc outAug ~ out,
    ProxToRef inpAnn env', ProxToRef outAug env') =>
    SF con inp out -> IO (HList prog', HList env', Ref inpAnn, Ref outAug)
toRearrangeable afrp = do
    (inprox, bs) <- fresh @_ @inp newFreshState (Proxy :: Proxy inp)
    (afrp', _, bs') <- assignMemory afrp inprox bs
    (prog, outprox') <- toMIO afrp' (Proxy :: Proxy inpAnn)
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