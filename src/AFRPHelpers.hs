module AFRPHelpers where

import AFRP
import Prelude hiding (id)

-- Bringing the constructors into scope to override the Control.Arrow constructors.
-- This might eventually be overloaded with a class.

id :: AFRP ArrowId a a
id = Id

(>>>) :: AFRP ar a b -> AFRP ar' b c -> AFRP (ArrowGGG ar ar') a c
f >>> g = f :>>>: g

(***) :: AFRP ar a b -> AFRP ar' a' b' -> AFRP (ArrowSSS ar ar') (P a a') (P b b')
f *** g = f :***: g

dup :: AFRP ArrowDup a (P a a)
dup = Dup

dropL :: AFRP ArrowDropL (P a b) b
dropL = DropL

dropR :: AFRP ArrowDropR (P a b) a
dropR = DropR

constant :: Val a -> AFRP (ArrowConst a) x a
constant = Constant

first :: AFRP ar a b -> AFRP (ArrowSSS ar ArrowId) (P a c) (P b c)
first = (*** id)

second :: AFRP ar a b -> AFRP (ArrowSSS ArrowId ar) (P c a) (P c b)
second = (id ***)

arr :: (Val a -> Val b) -> AFRP (ArrowArr b) a b
arr = Arr

-- A few helpers for arr:
arr11 :: (a -> b) -> AFRP (ArrowArr (V b)) (V a) (V b)
arr11 f = arr $ \(One x) -> One (f x)
arr21 :: (a -> b -> c) -> AFRP (ArrowArr (V c)) (P (V a) (V b)) (V c)
arr21 f = arr $ \(Pair (One x) (One y)) -> One (f x y)

app :: AFRP ArrowApp (P (V (AFRP ar b c)) b) c
app = App

loop :: AFRP ar (P a c) (P b c) -> AFRP (ArrowLoop ar c) a b
loop = Loop

(+++) :: AFRP ar a b -> AFRP ar' c d -> AFRP (ArrowPPP ar ar') (C a c) (C b d)
f +++ g = f :+++: g

(|||) :: AFRP ar a b -> AFRP ar' c b -> AFRP (ArrowGGG (ArrowPPP ar ar') (ArrowArr b)) (C a c) b
f ||| g = f +++ g >>> arr untag
    where
        untag :: Val (C b b) -> Val b
        untag (Choice1 v) = v
        untag (Choice2 v) = v

pre :: Val a -> AFRP ArrowPre a a
pre = Pre

-- A helper for pre:
pre1 :: a -> AFRP ArrowPre (V a) (V a)
pre1 a = Pre (One a)

returnA :: AFRP ArrowId a a
returnA = Id

-- Running functions for testing purposes.

runAFRP :: AFRP ar a b -> [Val a] -> [Val b]
runAFRP _ [] = []
runAFRP ga (x : xs) = let
    (y, ga') = run ga x
    in y : runAFRP ga' xs
    where
        run :: AFRP ar a b -> Val a -> (Val b, AFRP ar a b)
        run Id a = (a, Id)
        run DropL (Pair _ b) = (b, DropL)
        run DropR (Pair a _) = (a, DropR)
        run Dup a = (Pair a a, Dup)
        run (Constant v) _ = (v, Constant v)
        run (Arr f) a = (f a, Arr f)
        run (Pre v) v' = (v, Pre v')
        run (f :>>>: g) a = let
            (c, f') = run f a
            (b, g') = run g c
            in (b, f' :>>>: g')
        run (f :***: g) (Pair a b) = let
            (a', f') = run f a
            (b', g') = run g b
            in (Pair a' b', f' :***: g')
        run App (Pair (One f) x) = let
            (c, _) = run f x
            in (c, App)
        run (Loop f) a =
            let (Pair b c, f') = run f (Pair a c) in (b, Loop f')
        run (f :+++: g) (Choice1 a) =
            let (a', f') = run f a in (Choice1 a', f' :+++: g)
        run (f :+++: g) (Choice2 b) =
            let (b', g') = run g b in (Choice2 b', f :+++: g')