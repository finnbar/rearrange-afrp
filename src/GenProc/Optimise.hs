{-# LANGUAGE UndecidableInstances #-}

module GenProc.Optimise (optimise) where

import GenProc.GeneralisedArrow

import Unsafe.Coerce

-- Simple AFRP optimisations, to avoid e.g. chains of Arr.
-- We make use of unsafeCoerce since we know that e.g. ArrowId <=> we have Id and
-- nothing else, that is, each Arrow constructor has exactly one associated GenArrow
-- constructor. I'm sorry, Haskell gods.
-- NOTE: We can't use rewrite rules, as the type of the given GenArrow changes.

-- unsafeCoerce but with a stricter type signature.
coerceGenArrow :: GenArrow ar a b -> GenArrow ar' a b
coerceGenArrow = unsafeCoerce

type Optimise :: Arrow a b -> Arrow a b
type family Optimise arr where
    Optimise ArrowId = ArrowId
    Optimise ArrowDropL = ArrowDropL
    Optimise ArrowDropR = ArrowDropR
    Optimise ArrowDup = ArrowDup
    Optimise ArrowConst = ArrowConst
    Optimise ArrowArr = ArrowArr
    Optimise ArrowPre = ArrowPre
    Optimise (ArrowGGG l r b) = OptimiseComp (Optimise l) (Optimise r) b
    Optimise (ArrowSSS l r) = OptimisePair (Optimise l) (Optimise r)
    Optimise ArrowApp = ArrowApp
    Optimise (ArrowLoop ar c) = ArrowLoop (Optimise ar) c
    Optimise (ArrowPPP l r) = OptimiseChoice (Optimise l) (Optimise r)

-- Going through each combinator in turn, even if it remains unchanged.
optimise :: GenArrow ar a b -> GenArrow (Optimise ar) a b
optimise Id = Id
optimise DropL = DropL
optimise DropR = DropR
optimise Dup = Dup
optimise (Constant c) = Constant c
optimise (Arr f) = Arr f
optimise (Pre v) = Pre v
optimise (f :>>>: g) = optimiseComp (optimise f) (optimise g)
optimise (f :***: g) = optimisePair (optimise f) (optimise g)
optimise App = App
optimise (Loop f) = Loop (optimise f)
optimise (f :+++: g) = optimiseChoice (optimise f) (optimise g)

type OptimiseComp :: Arrow a b -> Arrow b c -> Desc b -> Arrow a c
type family OptimiseComp ar ar' desc where
    OptimiseComp ArrowArr ArrowArr _ = ArrowArr
    OptimiseComp ArrowDup ArrowDropL _ = ArrowId
    OptimiseComp ArrowDup ArrowDropR _ = ArrowId
    OptimiseComp _ ArrowConst _ = ArrowConst
    OptimiseComp ar ArrowId _ = ar
    OptimiseComp ArrowId ar _ = ar
    OptimiseComp ar ar' b = ArrowGGG ar ar' b

optimiseComp :: GenArrow ar a b -> GenArrow ar' b c -> GenArrow (OptimiseComp ar ar' b) a c
optimiseComp (Arr f) (Arr g) = Arr (g . f)
optimiseComp Dup DropL = Id
optimiseComp Dup DropR = Id
optimiseComp _ (Constant c) = Constant c
optimiseComp f Id = f
optimiseComp Id f = f
-- It cannot be any case of OptimiseComp except the last, so we can safely coerce.
optimiseComp f g = coerceGenArrow $ f :>>>: g

type OptimisePair :: Arrow a b -> Arrow a' b' -> Arrow (a ::: a') (b ::: b')
type family OptimisePair ar ar' where
    OptimisePair ArrowId ArrowId = ArrowId
    OptimisePair ArrowPre ArrowPre = ArrowPre
    OptimisePair ar ar' = ArrowSSS ar ar'

optimisePair :: GenArrow ar a b -> GenArrow ar' a' b'
    -> GenArrow (OptimisePair ar ar') (P a a') (P b b')
optimisePair Id Id = Id
optimisePair (Pre v) (Pre w) = Pre (Pair v w)
optimisePair l r = coerceGenArrow $ l :***: r

type OptimiseChoice :: Arrow a b -> Arrow a' b' -> Arrow (a :?: a') (b :?: b')
type family OptimiseChoice ar ar' where
    OptimiseChoice ArrowId ArrowId = ArrowId
    OptimiseChoice ar ar' = ArrowPPP ar ar'

optimiseChoice :: GenArrow ar a b -> GenArrow ar' a' b'
    -> GenArrow (OptimiseChoice ar ar') (C a a') (C b b')
optimiseChoice Id Id = Id
optimiseChoice l r = coerceGenArrow $ l :+++: r