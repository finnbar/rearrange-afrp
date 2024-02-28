{-# LANGUAGE FunctionalDependencies, UndecidableInstances, ScopedTypeVariables #-}

module Naming (AssignMemory(..), newFreshState, EmptyFreshKind, Fresh(..),
    FreshState(..), EnvFromFreshState, FreshKind) where

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

type FreshKind = (Nat, [*])
type FreshState :: FreshKind -> *
type family EnvFromFreshState (x :: FreshKind) where
    EnvFromFreshState '(n, env) = env
newtype FreshState (fs :: FreshKind) = MkFreshState (HList (EnvFromFreshState fs))

type Fresh :: forall s. Desc s -> FreshKind -> DescAnn s -> FreshKind -> Constraint
class Fresh d fk0 d' fk1 | d fk0 -> d' fk1 where
    -- Create a fresh undefined variable.
    fresh :: FreshState fk0 -> Proxy d
        -> IO (Proxy d', FreshState fk1)
    -- Create a fresh variable with the given value.
    freshInit :: FreshState fk0 -> Val d
        -> IO (Proxy d', FreshState fk1)

instance (n' ~ n + 1) =>
    Fresh (V a) '(n, env) (VN n a) '(n', Rearrange.Cell n a ': env) where
    fresh (MkFreshState ps) Proxy = do
        ref <- newIORef undefined
        let cell = Rearrange.Cell ref
        Prelude.return (Proxy, MkFreshState $ cell :+: ps)
    freshInit (MkFreshState ps) (One v) = do
        ref <- newIORef v
        let cell = Rearrange.Cell ref
        Prelude.return (Proxy, MkFreshState $ cell :+: ps)

instance (Fresh l fk0 lns fk1, Fresh r fk1 rns fk2) =>
    Fresh (P l r) fk0 (PN lns rns) fk2 where
    fresh bs prox = do
        let (lp, rp) = splitProx prox
        (lp', bs') <- fresh bs lp
        (rp', bs'') <- fresh bs' rp
        Prelude.return (pairProx lp' rp', bs'')
    freshInit bs (Pair l r) = do
        (lp, bs') <- freshInit bs l
        (rp, bs'') <- freshInit bs' r
        Prelude.return (pairProx lp rp, bs'')

type EmptyFreshKind = '(0, '[])
newFreshState :: FreshState EmptyFreshKind
newFreshState = MkFreshState HNil

-- The challenge with naming is that loops create cycles.
-- We therefore assignMemory as normal with various names, and then unify those that should
-- be the same with Substitute.
type AssignMemory :: Con sk sk' -> Desc sk -> Desc sk'
    -> ConAnn sk sk' -> DescAnn sk -> DescAnn sk'
    -> FreshKind -> FreshKind -> Constraint
class AssignMemory con inp out conAnn inpAnn outAnn fk0 fk1
    | con inp out inpAnn fk0 -> conAnn outAnn fk1 where
    assignMemory :: SF con inp out -> Proxy inpAnn -> FreshState fk0 ->
        IO (SFAnn conAnn inpAnn outAnn, Proxy outAnn, FreshState fk1)

instance AssignMemory IdCon inp inp IdCon' inpAnn inpAnn fk0 fk0 where
    assignMemory Id prox bs = return (Id', prox, bs)

instance AssignMemory DropLCon (P inpL inpR) inpR
    DropLCon' (PN inpLAnn inpRAnn) inpRAnn fk0 fk0 where
    
    assignMemory DropL prox bs = let (_, r) = splitProx prox in return (DropL', r, bs)

instance AssignMemory DropRCon (P inpL inpR) inpL
    DropRCon' (PN inpLAnn inpRAnn) inpLAnn fk0 fk0 where
    
    assignMemory DropR prox bs = let (l, _) = splitProx prox in return (DropR', l, bs)

instance AssignMemory DupCon inp (P inp inp) DupCon' inpAnn (PN inpAnn inpAnn) fk0 fk0 where
    assignMemory Dup prox bs = return (Dup', pairProx prox prox, bs)

instance (Fresh out fk0 outAnn fk1, inp ~ AsDesc inpAnn, out ~ AsDesc outAnn) =>
    AssignMemory ConstCon inp out (ConstCon' outAnn) inpAnn outAnn fk0 fk1 where
    assignMemory (Constant x) _ bs = do
        (prox', bs') <- freshInit bs x
        return (Constant' x, prox', bs')

instance (Fresh out fk0 outAnn fk1, inp ~ AsDesc inpAnn, out ~ AsDesc outAnn) =>
    AssignMemory ArrCon inp out ArrCon' inpAnn outAnn fk0 fk1 where
    assignMemory (Arr f) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy out)
        return (Arr' f, prox', bs')

instance (Fresh inp fk0 outAnn fk1, inp ~ AsDesc inpAnn, inp ~ AsDesc outAnn) =>
    AssignMemory PreCon inp inp PreCon' inpAnn outAnn fk0 fk1 where
    assignMemory (Pre v) _ bs = do
        (prox', bs') <- freshInit bs v
        return (Pre' v, prox', bs')

instance (AssignMemory con1 inp mid conAnn1 inpAnn midAnn fk0 fk1,
    AssignMemory con2 mid out conAnn2 midAnn outAnn fk1 fk2) =>
    AssignMemory (GGGCon con1 con2 mid) inp out
        (GGGCon' conAnn1 conAnn2 midAnn) inpAnn outAnn fk0 fk2 where
    
    assignMemory (f :>>>: g) prox bs = do
        (f', prox', bs') <- assignMemory f prox bs
        (g', prox'', bs'') <- assignMemory g prox' bs'
        return (f' :>>>:: g', prox'', bs'')

instance (AssignMemory con1 inpL outL conAnn1 inpLAnn outLAnn fk0 fk1,
    AssignMemory con2 inpR outR conAnn2 inpRAnn outRAnn fk1 fk2) =>
    AssignMemory (SSSCon con1 con2) (P inpL inpR) (P outL outR)
    (SSSCon' conAnn1 conAnn2) (PN inpLAnn inpRAnn) (PN outLAnn outRAnn) fk0 fk2 where
    assignMemory (f :***: g) prox bs = do
        let (lp, rp) = splitProx prox
        (f', lp', bs') <- assignMemory f lp bs
        (g', rp', bs'') <- assignMemory g rp bs'
        return (f' :***:: g', pairProx lp' rp', bs'')

-- For loops:
-- Make Fresh variables for the second input, use them to get the second output and then unify.
instance (Fresh looped fk0 loopedAnn fk1,
    -- AssignMemory the body once with the updated input DescAnn
    AssignMemory con (P inp looped) (P out looped)
        conAnn (PN inpAnn loopedAnn) (PN outAnn newLoopedAnn) fk1 fk2,
    -- Then repeatedly substitute until the looped desc is constant.
    RepeatSub conAnn inpAnn outAnn loopedAnn newLoopedAnn
        conFinal outFinal loopedFinal (loopedAnn == newLoopedAnn)) =>
    AssignMemory (LoopCon con looped) inp out (LoopCon' conFinal loopedFinal) inpAnn outFinal fk0 fk2 where
    assignMemory (Loop f) prox bs = do
        (proxc, bs') <- fresh bs (Proxy :: Proxy looped)
        (f', Proxy, bs'') <- assignMemory f (pairProx prox proxc) bs'
        let f'' = repeatSub (Proxy :: Proxy (loopedAnn == newLoopedAnn)) f'
        return (Loop' f'', Proxy :: Proxy outFinal, bs'')

class RepeatSub conAnn inpAnn outAnn loopedAnn newLoopedAnn
    conFinal outFinal loopedFinal eq
    | conAnn inpAnn outAnn loopedAnn newLoopedAnn eq -> conFinal outFinal loopedFinal where
    repeatSub :: Proxy eq -> SFAnn conAnn (PN inpAnn loopedAnn) (PN outAnn newLoopedAnn)
        -> SFAnn conFinal (PN inpAnn loopedFinal) (PN outFinal loopedFinal)

-- If we already have something of the right form, we are done.
instance RepeatSub con inp out looped looped con out looped 'True where
    repeatSub _ afrp = afrp

-- If not, we must go again.
instance (sub ~ ToSubstitution looped newLooped,
    Substitute con (PN inp looped) (PN out newLooped)
        conSub (PN inp loopedSub) (PN outSub newLoopedSub) sub,
    -- Then recursively call:
    RepeatSub conSub inp outSub loopedSub newLoopedSub
        conFinal outFinal loopedFinal (loopedSub == newLoopedSub)) =>
    RepeatSub con inp out looped newLooped conFinal outFinal loopedFinal 'False where
    
    repeatSub Proxy afrp =
        repeatSub (Proxy :: Proxy (loopedSub == newLoopedSub)) (substitute afrp (Proxy :: Proxy sub))

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

instance (Substitute con1 a b con1' a' b' sub, Substitute con2 b c con2' b' c' sub) =>
    Substitute (GGGCon' con1 con2 b) a c (GGGCon' con1' con2' b') a' c' sub where
    substitute (f :>>>:: g) sub = substitute f sub :>>>:: substitute g sub

instance (Substitute con1 a b con1' a' b' sub, Substitute con2 c d con2' c' d' sub) =>
    Substitute (SSSCon' con1 con2) (PN a c) (PN b d) (SSSCon' con1' con2') (PN a' c') (PN b' d') sub where
    substitute (f :***:: g) sub = substitute f sub :***:: substitute g sub

instance (Substitute con (PN a c) (PN b c) con' (PN a' c') (PN b' c') sub) =>
    Substitute (LoopCon' con c) a b (LoopCon' con' c') a' b' sub where
    substitute (Loop' f) sub = Loop' (substitute f sub)