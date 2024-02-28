{-# LANGUAGE UndecidableInstances #-}

module GenProc.Optimise (optimise, coerceSF) where

import AFRP

import Unsafe.Coerce

-- Simple AFRP optimisations, to avoid e.g. chains of Arr.
-- We make use of unsafeCoerce since we know that e.g. IdCon <=> we have Id and
-- nothing else, that is, each Con constructor has exactly one associated SF
-- constructor. I'm sorry, Haskell gods.
-- NOTE: We can't use rewrite rules, as the type of the given SF changes.

-- unsafeCoerce but with a stricter type signature.
coerceSF :: SF ar a b -> SF ar' a b
coerceSF = unsafeCoerce

type Optimise :: Con a b -> Con a b
type family Optimise arr where
    Optimise IdCon = IdCon
    Optimise DropLCon = DropLCon
    Optimise DropRCon = DropRCon
    Optimise DupCon = DupCon
    Optimise ConstCon = ConstCon
    Optimise ArrCon = ArrCon
    Optimise PreCon = PreCon
    Optimise (GGGCon l r b) = OptimiseComp (Optimise l) (Optimise r) b
    Optimise (SSSCon l r) = OptimisePair (Optimise l) (Optimise r)
    Optimise (LoopCon ar c) = LoopCon (Optimise ar) c

-- Going through each combinator in turn, even if it remains unchanged.
optimise :: SF ar a b -> SF (Optimise ar) a b
optimise Id = Id
optimise DropL = DropL
optimise DropR = DropR
optimise Dup = Dup
optimise (Constant c) = Constant c
optimise (Arr f) = Arr f
optimise (Pre v) = Pre v
optimise (f :>>>: g) = optimiseComp (optimise f) (optimise g)
optimise (f :***: g) = optimisePair (optimise f) (optimise g)
optimise (Loop f) = Loop (optimise f)

type OptimiseComp :: Con a b -> Con b c -> Desc b -> Con a c
type family OptimiseComp ar ar' b where
    OptimiseComp ArrCon ArrCon _ = ArrCon
    OptimiseComp DupCon DropLCon _ = IdCon
    OptimiseComp DupCon DropRCon _ = IdCon
    OptimiseComp _ ConstCon _ = ConstCon
    OptimiseComp ar IdCon _ = ar
    OptimiseComp IdCon ar _ = ar
    OptimiseComp ar ar' b = GGGCon ar ar' b

optimiseComp :: SF ar a b -> SF ar' b c -> SF (OptimiseComp ar ar' b) a c
optimiseComp (Arr f) (Arr g) = Arr (g . f)
optimiseComp Dup DropL = Id
optimiseComp Dup DropR = Id
optimiseComp _ (Constant c) = Constant c
optimiseComp f Id = f
optimiseComp Id f = f
-- It cannot be any case of OptimiseComp except the last, so we can safely coerce.
optimiseComp f g = coerceSF $ f :>>>: g

type OptimisePair :: Con a b -> Con a' b' -> Con (a ::: a') (b ::: b')
type family OptimisePair ar ar' where
    OptimisePair IdCon IdCon = IdCon
    OptimisePair PreCon PreCon = PreCon
    OptimisePair ar ar' = SSSCon ar ar'

optimisePair :: SF ar a b -> SF ar' a' b'
    -> SF (OptimisePair ar ar') (P a a') (P b b')
optimisePair Id Id = Id
optimisePair (Pre v) (Pre w) = Pre (Pair v w)
optimisePair l r = coerceSF $ l :***: r
