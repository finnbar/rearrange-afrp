{-# LANGUAGE FunctionalDependencies, UndecidableInstances, ScopedTypeVariables #-}

module Naming (fromAFRP) where

-- AIM: Transform an AFRP program into one with names (AFRP').

import GHC.TypeLits
import Data.Type.HList
import Data.Kind (Constraint)
import Data.IORef
import Data.Proxy
import Data.Type.Equality

import qualified Rearrange
import AFRP

-- Build an AFRP' from an AFRP

type EmptyDesc' :: Desc d -> Desc' d
type family EmptyDesc' c where
    EmptyDesc' (V a) = VN Nothing a
    EmptyDesc' (P l r) = PN (EmptyDesc' l) (EmptyDesc' r)

-- TODO I wonder if this typeclass can be avoided?
-- Should probably move to a new file at least.
class ToAFRP' arr a b arr' a' b' | arr a a' b -> arr' b' where
    toAFRP' :: AFRP arr a b -> DescProxy' a' -> (AFRP' arr' a' b', DescProxy' b')

instance ToAFRP' ArrowId a a ArrowId a' a' where
    toAFRP' Id prox = (Id', prox)

instance ToAFRP' ArrowDropL (P a b) b ArrowDropL (PN an bn) bn where
    toAFRP' DropL (PProx _ r) = (DropL', r)

instance ToAFRP' ArrowDropR (P a b) a ArrowDropR (PN an bn) an where
    toAFRP' DropR (PProx l _) = (DropR', l)

instance ToAFRP' ArrowDup a (P a a) ArrowDup an (PN an an) where
    toAFRP' Dup prox = (Dup', PProx prox prox)

instance (bn ~ EmptyDesc' b, b ~ AsDesc bn, a ~ AsDesc an, MakeDescProxy' bn)
    => ToAFRP' ArrowArr a b ArrowArr an bn where
    toAFRP' (Arr f) _ = (Arr' f, makeDescProxy' @bn)

instance (an' ~ EmptyDesc' a, a ~ AsDesc an', MakeDescProxy' an')
    => ToAFRP' ArrowPre a a ArrowPre an an' where
    toAFRP' (Pre v) _ = (Pre' v, makeDescProxy' @an')

instance (ToAFRP' arrl a b arrl' an bn, ToAFRP' arrr b c arrr' bn cn)
    => ToAFRP' (ArrowGGG arrl arrr b) a c (ArrowGGG arrl' arrr' bn) an cn where
    toAFRP' (f :>>>: g) prox = let
        (f', prox') = toAFRP' f prox
        (g', prox'') = toAFRP' g prox'
        in (f' :>>>>: g', prox'')

instance (ToAFRP' arrl a c arrl' an cn, ToAFRP' arrr b d arrr' bn dn)
    => ToAFRP' (ArrowSSS arrl arrr) (P a b) (P c d) (ArrowSSS arrl' arrr') (PN an bn) (PN cn dn) where
    toAFRP' (f :***: g) (PProx l r) = let
        (f', l') = toAFRP' f l
        (g', r') = toAFRP' g r
        in (f' :****: g', PProx l' r')

instance (MakeDescProxy' cn,
    cn ~ EmptyDesc' c, ToAFRP' arr (P a c) (P b c) arr' (PN an cn) (PN bn cn))
    => ToAFRP' (ArrowLoop arr c) a b (ArrowLoop arr' cn) an bn where
    toAFRP' (Loop f) prox = let
        (f', PProx out _) = toAFRP' f (PProx prox makeDescProxy')
        in (Loop' f', out)

-- Have the ability to make Fresh names to fill in the gaps in some Desc'.

type FreshState = (Nat, [*])
type BuildState :: FreshState -> *
type family EnvFromBuildState (x :: FreshState) where
    EnvFromBuildState '(n, env) = env
newtype BuildState (fs :: FreshState) = MkBuildState (HList (EnvFromBuildState fs))

type Fresh :: forall s. Desc' s -> FreshState -> Desc' s -> FreshState -> Constraint
class Fresh d fs d' fs' | d fs -> d' fs' where
    -- Create a fresh undefined variable.
    fresh :: BuildState fs -> DescProxy' d
        -> IO (DescProxy' d', BuildState fs')
    -- Create a fresh variable with some starting value.
    freshDefault :: BuildState fs -> DescProxy' d -> Val (AsDesc d)
        -> IO (DescProxy' d', BuildState fs')

instance (LookupNth n' env (Rearrange.IOCell n' a)) =>
    Fresh (VN (Just n') a) '(n, env) (VN (Just n') a) '(n, env) where
    fresh bs VProx = Prelude.return (VProx, bs)
    freshDefault bs@(MkBuildState ps) VProx v = do
        let ref = lookupNth (Proxy :: Proxy n') ps
        writeRef (VRef ref) v
        Prelude.return (VProx, bs)

instance (n' ~ n + 1) =>
    Fresh (VN Nothing a) '(n, env) (VN (Just n) a) '(n', Rearrange.IOCell n a ': env) where
    fresh (MkBuildState ps) VNoProx = do
        ref <- newIORef undefined
        let cell = Rearrange.Cell @_ @_ @IO ref
        Prelude.return (VProx, MkBuildState $ cell :+: ps)
    freshDefault (MkBuildState ps) VNoProx (One v) = do
        ref <- newIORef v
        let cell = Rearrange.Cell @_ @_ @IO ref
        Prelude.return (VProx, MkBuildState $ cell :+: ps)

instance (Fresh l fs lns fs', Fresh r fs' rns fs'') =>
    Fresh (PN l r) fs (PN lns rns) fs'' where
    fresh bs (PProx lp rp) = do
        (lp', bs') <- fresh bs lp
        (rp', bs'') <- fresh bs' rp
        Prelude.return (PProx lp' rp', bs'')
    freshDefault bs (PProx lp rp) (Pair l r) = do
        (lp', bs') <- freshDefault bs lp l
        (rp', bs'') <- freshDefault bs' rp r
        Prelude.return (PProx lp' rp', bs'')

-- The challenge with naming is that loops create cycles.
-- So we provide a way to propagate partial names and do this until we have named everything.

-- NOTE: Sources of complexity that I'd like to remove:
-- 1. AsDesc constraints. There's no way to remove this sadly.
-- 2. MakeDescProxy' constraints. (Is it simpler to just use Proxy?)

type Compile :: Arrow Desc' ar ar' -> Desc' ar -> Desc' ar' -> Arrow Desc' ar ar' ->
    Desc' ar -> Desc' ar' -> FreshState -> FreshState -> Constraint
class Compile arr a b arr' a' b' fs fs' | arr a b a' fs -> arr' b' fs' where
    compile :: AFRP' arr a b -> DescProxy' a' -> BuildState fs ->
        IO (AFRP' arr' a' b', DescProxy' b', BuildState fs')

instance Compile ArrowId a a ArrowId a' a' fs fs where
    compile Id' prox bs = return (Id', prox, bs)

instance Compile ArrowDropL (PN a b) b ArrowDropL (PN a' b') b' fs fs where
    compile DropL' (PProx _ r) bs = return (DropL', r, bs)

instance Compile ArrowDropR (PN a b) a ArrowDropR (PN a' b') a' fs fs where
    compile DropR' (PProx l _) bs = return (DropR', l, bs)

instance Compile ArrowDup a (PN a a) ArrowDup a' (PN a' a') fs fs where
    compile Dup' prox bs = return (Dup', PProx prox prox, bs)

instance (Fresh b fs b' fs', AsDesc a ~ AsDesc a', AsDesc b ~ AsDesc b', MakeDescProxy' b) =>
    Compile ArrowArr a b ArrowArr a' b' fs fs' where
        compile (Arr' f) _ bs = do
            (prox', bs') <- fresh bs (makeDescProxy' @b)
            return (Arr' f, prox', bs')

instance (Fresh b fs b' fs', AsDesc a ~ AsDesc a', AsDesc b ~ AsDesc b', MakeDescProxy' b) =>
    Compile ArrowPre a b ArrowPre a' b' fs fs' where
        compile (Pre' v) _ bs = do
            (prox', bs') <- freshDefault bs (makeDescProxy' @b) v
            return (Pre' v, prox', bs')

instance (Compile arrl a b arrl' a' b' fs fs', Compile arrr b c arrr' b' c' fs' fs'') =>
    Compile (ArrowGGG arrl arrr b) a c (ArrowGGG arrl' arrr' b') a' c' fs fs'' where
        compile (f :>>>>: g) prox bs = do
            (f', prox', bs') <- compile f prox bs
            (g', prox'', bs'') <- compile g prox' bs'
            return (f' :>>>>: g', prox'', bs'')

instance (Compile arrl a c arrl' a' c' fs fs',
    Compile arrr b d arrr' b' d' fs' fs'') =>
    Compile (ArrowSSS arrl arrr) (PN a b) (PN c d)
    (ArrowSSS arrl' arrr') (PN a' b') (PN c' d') fs fs'' where
        compile (f :****: g) (PProx lp rp) bs = do
            (f', lp', bs') <- compile f lp bs
            (g', rp', bs'') <- compile g rp bs'
            return (f' :****: g', PProx lp' rp', bs'')

-- For loops:
instance (MakeDescProxy' c,
    -- Compile the body once with the updated input Desc'
    Compile arr (PN a c) (PN b c) arr' (PN a' c) (PN b' d') fs fs',
    -- Then repeatedly compile it until the looped Desc' stays constant.
    CompileLoopBody arr' a' b' c d' arr'' b'' c'' fs' fs'' (c == d')) =>
    Compile (ArrowLoop arr c) a b (ArrowLoop arr'' c'') a' b'' fs fs'' where
        compile (Loop' f) prox bs = do
            compileRes <- compile f (PProx prox $ makeDescProxy' @c) bs
            (f', PProx prox' _, bs') <-
                compileLoopBody (Proxy :: Proxy (c == d')) compileRes
            return (Loop' f', prox', bs')

-- TODO: It feels like this can be simplified. We are performing a single unification.
-- Can we write a typeclass with a type-level unification idea?
-- Key idea is that AFRP' arr a b can be coerced to AFRP' arr a' b', where
-- AsDesc a ~ AsDesc a' and AsDesc b ~ AsDesc b'.
-- This also allows us to not do ToAFRP': we can perform a single pass from Desc to Desc'
-- which makes a call to unify.

-- Run compile repeatedly until the looped types line up.
-- The challenge is that the constraints differ based on the result:
-- If we compile once and get c ~ d, then we're done, but if not then we need to
-- perform compile again. This requires a trick like in the Partial eval stuff.

-- Errors with Nothing descriptors can be caught later if needed.
-- (It is impossible for an arr to have a Nothing entry as input, since
-- that means there is no start point for the wire coming in as input.)

-- Given the result of a compile, return if c~d, otherwise run again.
-- NOTE The input type `a` cannot change.
class CompileLoopBody arr a b c d arr' b' c' fs fs' done
    | arr a b c d fs done -> arr' c' b' fs' where
    compileLoopBody :: Proxy done ->
        (AFRP' arr (PN a c) (PN b d), DescProxy' (PN b d), BuildState fs) ->
        IO (AFRP' arr' (PN a c') (PN b' c'), DescProxy' (PN b' c'), BuildState fs')

-- If we already have something of the right form, we are done.
instance CompileLoopBody arr a b c c arr b c fs fs 'True where
    compileLoopBody _ = return

-- If not, we must go again.
instance (MakeDescProxy' a,
    -- Perform one compilation with the new looped Desc'.
    Compile arr (PN a c) (PN b d) arr' (PN a d) (PN b' d') fs fs',
    -- Then recursively call:
    CompileLoopBody arr' a b' d d' arr'' b'' c'' fs' fs'' (c == d')) =>
    CompileLoopBody arr a b c d arr'' b'' c'' fs fs'' 'False where
    
    compileLoopBody Proxy (afrp, PProx _ looped, bs) = do
        compileRes <- compile afrp (PProx (makeDescProxy' @a) looped) bs
        compileLoopBody (Proxy :: Proxy (c == d')) compileRes
    
fromAFRP :: forall arr a b arr' a' b' arr'' b'' fs' fs''.
    (MakeDescProxy' (EmptyDesc' a), Fresh (EmptyDesc' a) '(0, '[]) a' fs',
    ToAFRP' arr a b arr' a' b',
    Compile arr' a' b' arr'' a' b'' fs' fs'') =>
    AFRP arr a b -> IO (AFRP' arr'' a' b'', HList (EnvFromBuildState fs''))
fromAFRP afrp = do
    (prox, bs') <-
        fresh (MkBuildState HNil :: BuildState '(0, '[])) (makeDescProxy' @(EmptyDesc' a))
    let (afrp', _) = toAFRP' afrp prox
    (afrp'', _, MkBuildState list) <- compile afrp' prox bs'
    return (afrp'', list)