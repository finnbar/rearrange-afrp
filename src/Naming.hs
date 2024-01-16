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
import Data.Type.Bool (If)

import qualified Rearrange
import AFRP

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
class AssignMemory arr a b arr' a' b' fs fs' | arr a b a' fs -> arr' b' fs' where
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

instance (Fresh b fs b' fs', a ~ AsDesc a', b ~ AsDesc b') =>
    AssignMemory ArrowArr a b ArrowArr' a' b' fs fs' where
    assignMemory (Arr f) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy b)
        return (Arr' f, prox', bs')

instance (Fresh b fs b' fs', a ~ AsDesc a', b ~ AsDesc b') =>
    AssignMemory ArrowPre a b ArrowPre' a' b' fs fs' where
    assignMemory (Pre v) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy b)
        return (Pre' v, prox', bs')

instance (AssignMemory arrl a b arrl' a' b' fs fs', AssignMemory arrr b c arrr' b' c' fs' fs'') =>
    AssignMemory (ArrowGGG arrl arrr b) a c (ArrowGGG' arrl' arrr' b') a' c' fs fs'' where
    assignMemory (f :>>>: g) prox bs = do
        (f', prox', bs') <- assignMemory f prox bs
        (g', prox'', bs'') <- assignMemory g prox' bs'
        return (f' :>>>:: g', prox'', bs'')

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
instance (sub ~ ToSubstitution c d,
    Substitute arr (PN a c) (PN b d) arr' (PN a c') (PN b' d') sub,
    -- Then recursively call:
    RepeatSub arr' a b' c' d' arr'' b'' c'' (c' == d')) =>
    RepeatSub arr a b c d arr'' b'' c'' 'False where
    
    repeatSub Proxy afrp =
        repeatSub (Proxy :: Proxy (c' == d')) (substitute afrp (Proxy :: Proxy sub))

-- Within x, substitutes all Nat in s' with their equivalent in s.
-- INVARIANT: SingleSub s s' => s < s'
-- NOTE There is a subtle requirement here: if we were instead to swap the condition, we could
-- accidentally overwrite variables from outside of the loop. We utilise the fact that the first
-- input of the loop will have a lower label n than the second input n' -- so if the second output
-- contains n then it won't be overwritten with n'. 
-- We can theoretically avoid this by performing the substitution over the entire program,
-- but this sounds like a typing nightmare due to the requirement on Loop' that the second input/
-- output agree.
-- Simple explanation: by substituting large for small, we guarantee to only be touching names
-- generated within the loop.
data Substitution = Sub Nat Nat

type ToSubstitution :: Desc' d -> Desc' d -> [Substitution]
type family ToSubstitution d d' where
    ToSubstitution (PN l r) (PN l' r') = Combine (ToSubstitution l l') (ToSubstitution r r')
    ToSubstitution (VN n a) (VN n' a) = If (n <=? n') '[Sub n n'] '[Sub n' n]

type ApplySub :: Desc' a -> Substitution -> Desc' a
type family ApplySub n sub where
    ApplySub (PN l r) sub = PN (ApplySub l sub) (ApplySub r sub)
    ApplySub (VN n a) (Sub n' n) = VN n' a
    ApplySub (VN n a) (Sub x y)  = VN n a

type Subst :: Desc' a -> [Substitution] -> Desc' a
type family Subst x sub where
    Subst desc (sub : xs) = Subst (ApplySub desc sub) xs
    Subst desc '[] = desc

class Substitute arr a b arr' a' b' sub | arr a b sub -> arr' a' b' where
    substitute :: AFRP' arr a b -> Proxy sub -> AFRP' arr' a' b'

instance (a' ~ Subst a sub) => Substitute ArrowId' a a ArrowId' a' a' sub where
    substitute Id' _ = Id'

instance (a' ~ Subst a sub, b' ~ Subst b sub) =>
    Substitute ArrowDropL' (PN a b) b ArrowDropL' (PN a' b') b' sub where
    substitute DropL' _ = DropL'

instance (a' ~ Subst a sub, b' ~ Subst b sub) =>
    Substitute ArrowDropR' (PN a b) a ArrowDropR' (PN a' b') a' sub where
    substitute DropR' _ = DropR'

instance (a' ~ Subst a sub) =>
    Substitute ArrowDup' a (PN a a) ArrowDup' a' (PN a' a') sub where
    substitute Dup' _ = Dup'

instance (a' ~ Subst a sub, b' ~ Subst b sub, AsDesc a ~ AsDesc a', AsDesc b ~ AsDesc b') =>
    Substitute ArrowArr' a b ArrowArr' a' b' sub where
    substitute (Arr' f) _ = Arr' f

instance (b ~ Subst a sub, b' ~ Subst a' sub, AsDesc a ~ AsDesc b, AsDesc a' ~ AsDesc b') =>
    Substitute ArrowPre' a a' ArrowPre' b b' sub where
    substitute (Pre' v) _ = Pre' v

instance (Substitute arrl a b arrl' a' b' sub, Substitute arrr b c arrr' b' c' sub) =>
    Substitute (ArrowGGG' arrl arrr b) a c (ArrowGGG' arrl' arrr' b') a' c' sub where
    substitute (f :>>>:: g) sub = substitute f sub :>>>:: substitute g sub

instance (Substitute arrl a b arrl' a' b' sub, Substitute arrr c d arrr' c' d' sub) =>
    Substitute (ArrowSSS' arrl arrr) (PN a c) (PN b d) (ArrowSSS' arrl' arrr') (PN a' c') (PN b' d') sub where
    substitute (f :***:: g) sub = substitute f sub :***:: substitute g sub

instance (Substitute arr (PN a c) (PN b c) arr' (PN a' c') (PN b' c') sub) =>
    Substitute (ArrowLoop' arr c) a b (ArrowLoop' arr' c') a' b' sub where
    substitute (Loop' f) sub = Loop' (substitute f sub)
