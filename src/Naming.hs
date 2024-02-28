{-# LANGUAGE FunctionalDependencies, UndecidableInstances, ScopedTypeVariables #-}

module Naming (AssignMemory(..), newBuildState, EmptyFreshState, Fresh(..),
    BuildState(..), EnvFromBuildState, FreshState) where

-- AssignMemory transforms an SF into an SFAnn.

import GHC.TypeLits
import Data.Type.HList
import Data.Kind (Constraint)
import Data.IORef
import Data.Proxy
import Data.Type.Equality

import qualified Rearrange
import AFRP

-- Have the ability to make Fresh names to fill in the gaps in some DescAnn.

type FreshState = (Nat, [*])
type BuildState :: FreshState -> *
type family EnvFromBuildState (x :: FreshState) where
    EnvFromBuildState '(n, env) = env
newtype BuildState (fs :: FreshState) = MkBuildState (HList (EnvFromBuildState fs))

type Fresh :: forall s. Desc s -> FreshState -> DescAnn s -> FreshState -> Constraint
class Fresh d fs d' fs' | d fs -> d' fs' where
    -- Create a fresh undefined variable.
    fresh :: BuildState fs -> Proxy d
        -> IO (Proxy d', BuildState fs')
    -- Create a fresh variable with the given value.
    freshInit :: BuildState fs -> Val d
        -> IO (Proxy d', BuildState fs')

instance (n' ~ n + 1) =>
    Fresh (V a) '(n, env) (VN n a) '(n', Rearrange.IOCell n a ': env) where
    fresh (MkBuildState ps) Proxy = do
        ref <- newIORef undefined
        let cell = Rearrange.Cell @_ @_ @IO ref
        Prelude.return (Proxy, MkBuildState $ cell :+: ps)
    freshInit (MkBuildState ps) (One v) = do
        ref <- newIORef v
        let cell = Rearrange.Cell @_ @_ @IO ref
        Prelude.return (Proxy, MkBuildState $ cell :+: ps)

instance (Fresh l fs lns fs', Fresh r fs' rns fs'') =>
    Fresh (P l r) fs (PN lns rns) fs'' where
    fresh bs prox = do
        let (lp, rp) = splitProx prox
        (lp', bs') <- fresh bs lp
        (rp', bs'') <- fresh bs' rp
        Prelude.return (pairProx lp' rp', bs'')
    freshInit bs (Pair l r) = do
        (lp, bs') <- freshInit bs l
        (rp, bs'') <- freshInit bs' r
        Prelude.return (pairProx lp rp, bs'')

type EmptyFreshState = '(0, '[])
newBuildState :: BuildState EmptyFreshState
newBuildState = MkBuildState HNil

-- The challenge with naming is that loops create cycles.
-- We therefore assignMemory as normal with various names, and then unify those that should
-- be the same with Substitute.
type AssignMemory :: Con ar ar' -> Desc ar -> Desc ar'
    -> ConAnn ar ar' -> DescAnn ar -> DescAnn ar'
    -> FreshState -> FreshState -> Constraint
class AssignMemory arr a b arr' a' b' fs fs' | arr a b a' fs -> arr' b' fs' where
    assignMemory :: SF arr a b -> Proxy a' -> BuildState fs ->
        IO (SFAnn arr' a' b', Proxy b', BuildState fs')

instance AssignMemory IdCon a a IdCon' a' a' fs fs where
    assignMemory Id prox bs = return (Id', prox, bs)

instance AssignMemory DropLCon (P a b) b DropLCon' (PN a' b') b' fs fs where
    assignMemory DropL prox bs = let (_, r) = splitProx prox in return (DropL', r, bs)

instance AssignMemory DropRCon (P a b) a DropRCon' (PN a' b') a' fs fs where
    assignMemory DropR prox bs = let (l, _) = splitProx prox in return (DropR', l, bs)

instance AssignMemory DupCon a (P a a) DupCon' a' (PN a' a') fs fs where
    assignMemory Dup prox bs = return (Dup', pairProx prox prox, bs)

instance (Fresh a fs a' fs', x ~ AsDesc x', a ~ AsDesc a') =>
    AssignMemory ConstCon x a (ConstCon' a') x' a' fs fs' where
    assignMemory (Constant x) _ bs = do
        (prox', bs') <- freshInit bs x
        return (Constant' x, prox', bs')

instance (Fresh b fs b' fs', a ~ AsDesc a', b ~ AsDesc b') =>
    AssignMemory ArrCon a b ArrCon' a' b' fs fs' where
    assignMemory (Arr f) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy b)
        return (Arr' f, prox', bs')

instance (Fresh a fs b' fs', a ~ AsDesc a', a ~ AsDesc b') =>
    AssignMemory PreCon a a PreCon' a' b' fs fs' where
    assignMemory (Pre v) _ bs = do
        (prox', bs') <- freshInit bs v
        return (Pre' v, prox', bs')

instance (AssignMemory arrl a b arrl' a' b' fs fs', AssignMemory arrr b c arrr' b' c' fs' fs'') =>
    AssignMemory (GGGCon arrl arrr b) a c (GGGCon' arrl' arrr' b') a' c' fs fs'' where
    assignMemory (f :>>>: g) prox bs = do
        (f', prox', bs') <- assignMemory f prox bs
        (g', prox'', bs'') <- assignMemory g prox' bs'
        return (f' :>>>:: g', prox'', bs'')

instance (AssignMemory arrl a c arrl' a' c' fs fs',
    AssignMemory arrr b d arrr' b' d' fs' fs'') =>
    AssignMemory (SSSCon arrl arrr) (P a b) (P c d)
    (SSSCon' arrl' arrr') (PN a' b') (PN c' d') fs fs'' where
    assignMemory (f :***: g) prox bs = do
        let (lp, rp) = splitProx prox
        (f', lp', bs') <- assignMemory f lp bs
        (g', rp', bs'') <- assignMemory g rp bs'
        return (f' :***:: g', pairProx lp' rp', bs'')

-- For loops:
-- Make Fresh variables for the second input, use them to get the second output and then unify.
instance (Fresh c fs c' fs',
    -- AssignMemory the body once with the updated input DescAnn
    AssignMemory arr (P a c) (P b c) arr' (PN a' c') (PN b' d') fs' fs'',
    -- Then repeatedly substitute until the looped desc is constant.
    RepeatSub arr' a' b' c' d' arr'' b'' c'' (c' == d')) =>
    AssignMemory (LoopCon arr c) a b (LoopCon' arr'' c'') a' b'' fs fs'' where
    assignMemory (Loop f) prox bs = do
        (proxc, bs') <- fresh bs (Proxy :: Proxy c)
        (f', Proxy, bs'') <- assignMemory f (pairProx prox proxc) bs'
        let f'' = repeatSub (Proxy :: Proxy (c' == d')) f'
        return (Loop' f'', Proxy :: Proxy b'', bs'')

class RepeatSub arr a b c d arr' b' c' eq | arr a b c d eq -> arr' b' c' where
    repeatSub :: Proxy eq -> SFAnn arr (PN a c) (PN b d) -> SFAnn arr' (PN a c') (PN b' c')

-- If we already have something of the right form, we are done.
instance RepeatSub arr a b c c arr b c 'True where
    repeatSub _ afrp = afrp

-- If not, we must go again.
instance (sub ~ ToSubstitution c d,
    Substitute arr (PN a c) (PN b d) arr' (PN a c') (PN b' d') sub,
    -- Then recursively call:
    RepeatSub arr' a b' c' d' arr'' b'' c'' (c' == d')) =>
    RepeatSub arr a b c d arr'' b'' c'' 'False where
    
    repeatSub Proxy afrp =
        repeatSub (Proxy :: Proxy (c' == d')) (substitute afrp (Proxy :: Proxy sub))

-- Within x, substitutes all Nat in s' with their equivalent in s.
-- Important note: we always substitute output name in place of input name.
-- This is to avoid replacing something defined outside of the loop, which could be a loop output.
-- Sub a b => whenever you see a, replace it with b
data Substitution = Sub Nat Nat

type ToSubstitution :: DescAnn d -> DescAnn d -> [Substitution]
type family ToSubstitution d d' where
    ToSubstitution (PN l r) (PN l' r') = Combine (ToSubstitution l l') (ToSubstitution r r')
    -- Whenever you see the input name n, replace it with the output name n'.
    ToSubstitution (VN n a) (VN n' a) = '[Sub n n']

type ApplySub :: DescAnn a -> Substitution -> DescAnn a
type family ApplySub n sub where
    ApplySub (PN l r) sub = PN (ApplySub l sub) (ApplySub r sub)
    ApplySub (VN n a) (Sub n n') = VN n' a
    ApplySub (VN n a) (Sub x y)  = VN n a

type Subst :: DescAnn a -> [Substitution] -> DescAnn a
type family Subst x sub where
    Subst desc (sub : xs) = Subst (ApplySub desc sub) xs
    Subst desc '[] = desc

class Substitute arr a b arr' a' b' sub | arr a b sub -> arr' a' b' where
    substitute :: SFAnn arr a b -> Proxy sub -> SFAnn arr' a' b'

instance (a' ~ Subst a sub) => Substitute IdCon' a a IdCon' a' a' sub where
    substitute Id' _ = Id'

instance (a' ~ Subst a sub, b' ~ Subst b sub) =>
    Substitute DropLCon' (PN a b) b DropLCon' (PN a' b') b' sub where
    substitute DropL' _ = DropL'

instance (a' ~ Subst a sub, b' ~ Subst b sub) =>
    Substitute DropRCon' (PN a b) a DropRCon' (PN a' b') a' sub where
    substitute DropR' _ = DropR'

instance (a' ~ Subst a sub) =>
    Substitute DupCon' a (PN a a) DupCon' a' (PN a' a') sub where
    substitute Dup' _ = Dup'

instance (a' ~ Subst a sub, b' ~ Subst b sub, AsDesc a ~ AsDesc a', AsDesc b ~ AsDesc b') =>
    Substitute ArrCon' a b ArrCon' a' b' sub where
    substitute (Arr' f) _ = Arr' f

instance (b ~ Subst a sub, b' ~ Subst a' sub, AsDesc a ~ AsDesc b, AsDesc a' ~ AsDesc b') =>
    Substitute PreCon' a a' PreCon' b b' sub where
    substitute (Pre' v) _ = Pre' v

instance (Substitute arrl a b arrl' a' b' sub, Substitute arrr b c arrr' b' c' sub) =>
    Substitute (GGGCon' arrl arrr b) a c (GGGCon' arrl' arrr' b') a' c' sub where
    substitute (f :>>>:: g) sub = substitute f sub :>>>:: substitute g sub

instance (Substitute arrl a b arrl' a' b' sub, Substitute arrr c d arrr' c' d' sub) =>
    Substitute (SSSCon' arrl arrr) (PN a c) (PN b d) (SSSCon' arrl' arrr') (PN a' c') (PN b' d') sub where
    substitute (f :***:: g) sub = substitute f sub :***:: substitute g sub

instance (Substitute arr (PN a c) (PN b c) arr' (PN a' c') (PN b' c') sub) =>
    Substitute (LoopCon' arr c) a b (LoopCon' arr' c') a' b' sub where
    substitute (Loop' f) sub = Loop' (substitute f sub)