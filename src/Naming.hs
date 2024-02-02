{-# LANGUAGE FunctionalDependencies, UndecidableInstances, ScopedTypeVariables #-}

module Naming (AssignMemory(..), newBuildState, EmptyFreshState, Fresh(..),
    BuildState(..), EnvFromBuildState, FreshState) where

-- AssignMemory transforms an AFRP into an AFRP'.

import GHC.TypeLits
import Data.Type.HList
import Data.Kind (Constraint)
import Data.IORef
import Data.Proxy
import Data.Type.Equality

import qualified Rearrange
import AFRP
import Unsafe.Coerce

-- Have the ability to make Fresh names to fill in the gaps in some Desc'.

type FreshState = (Nat, [*])
type BuildState :: FreshState -> *
type family EnvFromBuildState (x :: FreshState) where
    EnvFromBuildState '(n, env) = env
newtype BuildState (fs :: FreshState) = MkBuildState (HList (EnvFromBuildState fs))

type Fresh :: forall s. Desc s -> FreshState -> Desc' s -> FreshState -> Constraint
class Fresh d fs d' fs' | d fs -> d' fs' where
    -- Create a fresh undefined variable.
    fresh :: BuildState fs -> Proxy d
        -> IO (Proxy d', BuildState fs')

instance (n' ~ n + 1, env' ~ Append (Rearrange.IOCell n a) env) =>
    Fresh (V a) '(n, env) (VN n a) '(n', env') where
    fresh (MkBuildState ps) Proxy = do
        ref <- newIORef undefined
        let cell = Rearrange.Cell @_ @_ @IO ref
        Prelude.return (Proxy, MkBuildState $ hAppend @(Rearrange.IOCell n a) cell ps)

instance (Fresh l fs lns fs', Fresh r fs' rns fs'') =>
    Fresh (P l r) fs (PN lns rns) fs'' where
    fresh bs prox = do
        let (lp, rp) = splitProx prox
        (lp', bs') <- fresh bs lp
        (rp', bs'') <- fresh bs' rp
        Prelude.return (pairProx lp' rp', bs'')

type EmptyFreshState = '(0, '[])
newBuildState :: BuildState EmptyFreshState
newBuildState = MkBuildState HNil

-- The challenge with naming is that loops create cycles.
-- We therefore assignMemory as normal with various names, and then unify those that should
-- be the same with Substitute.
type AssignMemory :: Arrow ar ar' -> Desc ar -> Desc ar'
    -> Arrow' ar ar' -> Desc' ar -> Desc' ar'
    -> FreshState -> FreshState -> Constraint
class AssignMemory arr a b arr' a' b' fs fs' | arr a a' fs -> arr' b b' fs' where
    assignMemory :: AFRP arr a b -> Proxy a' -> BuildState fs ->
        IO (AFRP' arr' a' b', Proxy b', BuildState fs')

instance AssignMemory ArrowId a a ArrowId' a' a' fs fs where
    assignMemory Id prox bs = return (Id', prox, bs)

instance AssignMemory ArrowDropL (P a b) b ArrowDropL' (PN a' b') b' fs fs where
    assignMemory DropL prox bs = let (_, r) = splitProx prox in return (DropL', r, bs)

instance AssignMemory ArrowDropR (P a b) a ArrowDropR' (PN a' b') a' fs fs where
    assignMemory DropR prox bs = let (l, _) = splitProx prox in return (DropR', l, bs)

instance AssignMemory ArrowDup a (P a a) ArrowDup' a' (PN a' a') fs fs where
    assignMemory Dup prox bs = return (Dup', pairProx prox prox, bs)

instance (Fresh a fs a' fs', x ~ AsDesc x', a ~ AsDesc a') =>
    AssignMemory (ArrowConst a) x a (ArrowConst' a') x' a' fs fs' where
    assignMemory (Constant x) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy a)
        return (Constant' x, prox', bs')

instance (Fresh b fs b' fs', a ~ AsDesc a', b ~ AsDesc b') =>
    AssignMemory (ArrowArr b) a b (ArrowArr' b') a' b' fs fs' where
    assignMemory (Arr f) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy b)
        return (Arr' f, prox', bs')

instance (Fresh a fs b' fs', a ~ AsDesc a', a ~ AsDesc b') =>
    AssignMemory ArrowPre a a (ArrowPre' b') a' b' fs fs' where
    assignMemory (Pre v) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy a)
        return (Pre' v, prox', bs')

instance (AssignMemory arrl a b arrl' a' b' fs fs', AssignMemory arrr b c arrr' b' c' fs' fs'') =>
    AssignMemory (ArrowGGG arrl arrr) a c (ArrowGGG' arrl' arrr') a' c' fs fs'' where
    assignMemory fg prox bs = do
        let (f, g) = splitGGG fg
        (f', prox', bs') <- assignMemory f prox bs
        (g', prox'', bs'') <- assignMemory g prox' bs'
        return (f' :>>>:: g', prox'', bs'')
    
-- NOTE: We unsafeCoerce here.
-- This is because we know that AssignMemory will pick the b corresponding to the b hidden within the :>>>: constructor,
-- but have no easy way to persuade Haskell of this.
splitGGG :: GenArrow (ArrowGGG arrl arrr) a c -> (GenArrow arrl a b, GenArrow arrr b c)
splitGGG (f :>>>: g) = (unsafeCoerce f, unsafeCoerce g)

instance (AssignMemory arrl a c arrl' a' c' fs fs',
    AssignMemory arrr b d arrr' b' d' fs' fs'') =>
    AssignMemory (ArrowSSS arrl arrr) (P a b) (P c d)
    (ArrowSSS' arrl' arrr') (PN a' b') (PN c' d') fs fs'' where
    assignMemory (f :***: g) prox bs = do
        let (lp, rp) = splitProx prox
        (f', lp', bs') <- assignMemory f lp bs
        (g', rp', bs'') <- assignMemory g rp bs'
        return (f' :***:: g', pairProx lp' rp', bs'')

-- For loops:
-- Make Fresh variables for the second input, use them to get the second output and then unify.
instance (Fresh c fs c' fs',
    -- AssignMemory the body once with the updated input Desc'
    AssignMemory arr (P a c) (P b c) arr' (PN a' c') (PN b' d') fs' fs'',
    -- Then repeatedly substitute until the looped desc is constant.
    RepeatSub arr' a' b' c' d' arr'' b'' c'' (c' == d')) =>
    AssignMemory (ArrowLoop arr c) a b (ArrowLoop' arr'' c'') a' b'' fs fs'' where
    assignMemory (Loop f) prox bs = do
        (proxc, bs') <- fresh bs (Proxy :: Proxy c)
        (f', Proxy, bs'') <- assignMemory f (pairProx prox proxc) bs'
        let f'' = repeatSub (Proxy :: Proxy (c' == d')) f'
        return (Loop' f'', Proxy :: Proxy b'', bs'')

class RepeatSub arr a b c d arr' b' c' eq | arr a b c d eq -> arr' b' c' where
    repeatSub :: Proxy eq -> AFRP' arr (PN a c) (PN b d) -> AFRP' arr' (PN a c') (PN b' c')

-- If we already have something of the right form, we are done.
instance RepeatSub arr a b c c arr b c 'True where
    repeatSub _ afrp = afrp

-- If not, we must go again.
instance (ir ~ MakeInputRename d,
    UpdateInputs arr (PN a c) (PN b d) arr' (PN a c') (PN b' d') (PairIR EmptyIR ir) ir',
    -- Then recursively call:
    RepeatSub arr' a b' c' d' arr'' b'' c'' (c' == d')) =>
    RepeatSub arr a b c d arr'' b'' c'' 'False where
    
    repeatSub Proxy afrp = let
        (afrp', _) = updateInputs afrp (Proxy :: Proxy (PairIR EmptyIR ir))
        in repeatSub (Proxy :: Proxy (c' == d')) afrp'

-- SIMPLER SUBSTITUTION
-- Rather than doing a name-based substitution, we do an inputs-based one.

data InputRename sk where
    Renamed :: Nat -> InputRename O
    Unchanged :: InputRename O
    PairIR :: InputRename l -> InputRename r -> InputRename (l ::: r)
    EmptyIR :: InputRename sk

type IRSubst :: Desc' sk -> InputRename sk -> Desc' sk
type family IRSubst desc' ir where
    IRSubst (VN n a) (Renamed n') = VN n' a
    IRSubst (VN n a) Unchanged = VN n a
    IRSubst (PN l r) (PairIR il ir) = PN (IRSubst l il) (IRSubst r ir)
    IRSubst t EmptyIR = t

type MakeInputRename :: Desc' sk -> InputRename sk
type family MakeInputRename desc' where
    MakeInputRename (VN n a) = Renamed n
    MakeInputRename (PN l r) = PairIR (MakeInputRename l) (MakeInputRename r)

type UpdateInputs :: Arrow' sk sk' -> Desc' sk -> Desc' sk' -> Arrow' sk sk' -> Desc' sk -> Desc' sk' -> InputRename sk -> InputRename sk' -> Constraint
class UpdateInputs ar a b ar' a' b' ir ir' | ar a ir -> ar' b a' b' ir' where
    updateInputs :: AFRP' ar a b -> Proxy ir -> (AFRP' ar' a' b', Proxy ir')

instance (IRSubst a l ~ a', IRSubst b r ~ b') =>
    UpdateInputs ArrowDropL' (PN a b) b ArrowDropL' (PN a' b') b' (PairIR l r) r where
        updateInputs DropL' _ = (DropL', Proxy :: Proxy r)

instance (IRSubst a l ~ a', IRSubst b r ~ b') =>
    UpdateInputs ArrowDropR' (PN a b) a ArrowDropR' (PN a' b') a' (PairIR l r) l where
        updateInputs DropR' _ = (DropR', Proxy :: Proxy l)

instance (IRSubst a ir ~ a') =>
    UpdateInputs ArrowId' a a ArrowId' a' a' ir ir where
        updateInputs Id' prox = (Id', prox)

instance (IRSubst a ir ~ a') =>
    UpdateInputs ArrowDup' a (PN a a) ArrowDup' a' (PN a' a') ir (PairIR ir ir) where
        updateInputs Dup' _ = (Dup', Proxy :: Proxy (PairIR ir ir))

instance (IRSubst a ir ~ a', AsDesc a ~ AsDesc a') =>
    UpdateInputs (ArrowArr' b) a b (ArrowArr' b) a' b ir EmptyIR where
        updateInputs (Arr' f) _ = (Arr' f, Proxy :: Proxy EmptyIR)

instance (IRSubst a ir ~ a', AsDesc a ~ AsDesc a') =>
    UpdateInputs (ArrowPre' b) a b (ArrowPre' b) a' b ir EmptyIR where
        updateInputs (Pre' v) _ = (Pre' v, Proxy :: Proxy EmptyIR)

instance (UpdateInputs arl a b arl' a' b' ir ir', UpdateInputs arr b c arr' b' c' ir' ir'') =>
    UpdateInputs (ArrowGGG' arl arr) a c (ArrowGGG' arl' arr') a' c' ir ir'' where
        updateInputs fg prox = let
            (f, g) = splitGGG' fg
            (f', prox') = updateInputs f prox
            (g', prox'') = updateInputs g prox'
            in (f' :>>>:: g', prox'')

splitGGG' :: AFRP' (ArrowGGG' arrl arrr) a c -> (AFRP' arrl a b, AFRP' arrr b c)
splitGGG' (f :>>>:: g) = (unsafeCoerce f, unsafeCoerce g)

instance (UpdateInputs arl a b arl' a' b' irl irl', UpdateInputs arr c d arr' c' d' irr irr') =>
    UpdateInputs (ArrowSSS' arl arr) (PN a c) (PN b d)
    (ArrowSSS' arl' arr') (PN a' c') (PN b' d') (PairIR irl irr) (PairIR irl' irr') where
        updateInputs (f :***:: g) _ = let
            (f', _) = updateInputs f (Proxy :: Proxy irl)
            (g', _) = updateInputs g (Proxy :: Proxy irr)
            in (f' :***:: g', Proxy :: Proxy (PairIR irl' irr'))

instance (UpdateInputs ar (PN a c) (PN b c) ar' (PN a' c') (PN b' c') (PairIR ir EmptyIR) (PairIR ir' EmptyIR)) =>
    UpdateInputs (ArrowLoop' ar c) a b (ArrowLoop' ar' c') a' b' ir ir' where
        updateInputs (Loop' f) _ = let
            (f', _) = updateInputs f (Proxy :: Proxy (PairIR ir EmptyIR))
            in (Loop' f', Proxy :: Proxy ir')