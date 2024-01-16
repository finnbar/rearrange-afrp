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

type CombineArr :: Arrow a b -> Arrow b c -> Desc b -> Arrow a c
type family CombineArr x y desc where
    CombineArr ArrowArr ArrowArr _ = ArrowArr
    CombineArr ArrowDup ArrowDropL _ = ArrowId
    CombineArr ArrowDup ArrowDropR _ = ArrowId
    CombineArr ar ArrowConst _ = ArrowConst
    CombineArr (ArrowSSS arl arr) (ArrowSSS arl' arr') (P l r) =
        ArrowSSS (CombineArr arl arl' l) (CombineArr arr arr' r)
    CombineArr ar ArrowId _ = ar
    CombineArr ArrowId ar _ = ar
    CombineArr (ArrowGGG arx ary b) arz b' = CombineArrCont (CombineArr arx ary b) arz b'
    CombineArr arx (ArrowGGG ary arz b') b = CombineArrCont' arx (CombineArr ary arz b') b
    CombineArr ar ar' b = ArrowGGG ar ar' b

type CombineArrCont :: Arrow a b -> Arrow b c -> Desc b -> Arrow a c
type family CombineArrCont x y desc where
    CombineArrCont (ArrowGGG arx ary b) arz b' = ArrowGGG arx (CombineArr ary arz b') b
    CombineArrCont ar arz b' = CombineArr ar arz b'

type CombineArrCont' :: Arrow a b -> Arrow b c -> Desc b -> Arrow a c
type family CombineArrCont' x y desc where
    CombineArrCont' arx (ArrowGGG ary arz b') b = ArrowGGG (CombineArr arx ary b) arz b'
    CombineArrCont' ar arz b' = CombineArr ar arz b'

optimiseComp :: GenArrow ar a b -> GenArrow ar' b c -> GenArrow (CombineArr ar ar' b) a c
optimiseComp (Arr f) (Arr g) = Arr (g . f)
optimiseComp Dup DropL = Id
optimiseComp Dup DropR = Id
optimiseComp _ (Constant c) = Constant c
optimiseComp (f :***: g) (f' :***: g') = optimiseComp f f' :***: optimiseComp g g'
optimiseComp f Id = f
optimiseComp Id f = f
-- Relies on knowledge that h !~ Id or Constant.
-- We know this since we have a case for those above, even if Haskell fails to spot it.
-- Therefore, we use unsafeCoerce.
optimiseComp (f :>>>: g) h = coerceGenArrow $ optimiseCompCont (optimiseComp f g) h
optimiseComp f (g :>>>: h) = coerceGenArrow $ optimiseCompCont' f (optimiseComp g h)
-- Relies on knowledge that none of our other cases apply.
optimiseComp f g = coerceGenArrow $ f :>>>: g

optimiseCompCont :: GenArrow ar a b -> GenArrow ar' b c
    -> GenArrow (CombineArrCont ar ar' b) a c
optimiseCompCont (f :>>>: g) h = f :>>>: optimiseComp g h
-- Again, to avoid enumerating cases.
optimiseCompCont fg h = coerceGenArrow $ optimiseComp fg h

optimiseCompCont' :: GenArrow ar a b -> GenArrow ar' b c
    -> GenArrow (CombineArrCont' ar ar' b) a c
optimiseCompCont' f (g :>>>: h) = optimiseComp f g :>>>: h
-- Again, to avoid enumerating cases.
optimiseCompCont' f gh = coerceGenArrow $ optimiseComp f gh

type Optimise :: Arrow a b -> Arrow a b
type family Optimise ar where
    Optimise (ArrowGGG ar ar' b) = CombineArr ar ar' b
    Optimise (ArrowSSS ar ar') = ArrowSSS (Optimise ar) (Optimise ar')
    Optimise ar = ar

optimise :: GenArrow ar a b -> GenArrow (Optimise ar) a b
optimise (f :>>>: g) = optimiseComp f g
optimise (f :***: g) = optimise f :***: optimise g
optimise ar = coerceGenArrow ar