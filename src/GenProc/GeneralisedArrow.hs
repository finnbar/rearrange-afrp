{-# LANGUAGE PolyKinds, StandaloneKindSignatures, RankNTypes, GADTs, TypeOperators,
    FlexibleInstances, FlexibleContexts  #-}

module GenProc.GeneralisedArrow where

import Data.Kind (Type)
import Prelude hiding (id)

-- We're using the standard SFRP setup rather than classes, as I couldn't think of a way to generalise things nicely.
-- We gain an additional arity and desc type, :?: and C respectively, which represents when we have a choice of signals.

data SKind where
    O :: SKind
    (:::) :: SKind -> SKind -> SKind
    (:?:) :: SKind -> SKind -> SKind

type Desc :: SKind -> Type
data Desc x where
    V :: forall (a :: Type). a -> Desc O
    P :: Desc a -> Desc b -> Desc (a ::: b)
    C :: Desc a -> Desc b -> Desc (a :?: b)

type Val :: forall s. Desc s -> Type
data Val x where
    One :: a -> Val (V a)
    Pair :: Val a -> Val b -> Val (P a b)
    Choice1 :: Val a -> Val (C a b)
    Choice2 :: Val b -> Val (C a b)

instance Show a => Show (Val (V a)) where
    show (One a) = show a

instance (Show (Val l), Show (Val r)) => Show (Val (P l r)) where
    show (Pair l r) = "[|" ++ show l ++ ", " ++ show r ++ "|]"

instance (Show (Val a), Show (Val b)) => Show (Val (C a b)) where
    show (Choice1 a) = "(1 " ++ show a ++ ")"
    show (Choice2 a) = "(2 " ++ show a ++ ")"

type Arrow :: SKind -> SKind -> Type
data Arrow ar ar' where
    ArrowId :: Arrow a a
    ArrowDropL :: Arrow (a ::: b) b
    ArrowDropR :: Arrow (a ::: b) a
    ArrowDup :: Arrow a (a ::: a)
    ArrowConst :: Arrow a b

    ArrowArr :: Arrow a b
    ArrowPre :: Arrow a a
    ArrowGGG :: Arrow a b -> Arrow b c -> Desc b -> Arrow a c
    ArrowSSS :: Arrow a b -> Arrow a' b' -> Arrow (a ::: a') (b ::: b')
    ArrowApp :: Arrow (O ::: a) b
    ArrowLoop :: Arrow (a ::: c) (b ::: c) -> Desc c -> Arrow a b
    ArrowPPP :: Arrow a b -> Arrow c d -> Arrow (a :?: c) (b :?: d)

-- Programmers will likely use :: GenArrow _ a b in their code, since _ is entirely inferrable from the constructors.
-- We need it to implement an GenArrow -> RGenArrow function, as the type of the output cannot solely depend on the value of the input.
-- (That's dependent types innit.)
type GenArrow :: forall (ar :: SKind) (ar' :: SKind).
    Arrow ar ar' -> Desc ar -> Desc ar' -> Type
data GenArrow arrow a b where
    -- Routing
    Id :: GenArrow ArrowId a a
    DropL :: GenArrow ArrowDropL (P a b) b
    DropR :: GenArrow ArrowDropR (P a b) a
    Dup :: GenArrow ArrowDup a (P a a)
    Constant :: Val a -> GenArrow ArrowConst x a
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr :: (Val a -> Val b) -> GenArrow ArrowArr a b
    Pre :: Val a -> GenArrow ArrowPre a a
    (:>>>:) :: GenArrow ar a b -> GenArrow ar' b c -> GenArrow (ArrowGGG ar ar' b) a c
    (:***:) :: GenArrow ar a b -> GenArrow ar' a' b' -> GenArrow (ArrowSSS ar ar') (P a a') (P b b')
    App :: GenArrow ArrowApp (P (V (GenArrow ar b c)) b) c
    Loop :: GenArrow ar (P a c) (P b c) -> GenArrow (ArrowLoop ar c) a b
    (:+++:) :: GenArrow ar a b -> GenArrow ar' c d -> GenArrow (ArrowPPP ar ar') (C a c) (C b d)

instance Show (GenArrow ar a b) where
    show Id = "Id"
    show DropL = "DropL"
    show DropR = "DropR"
    show Dup = "Dup"
    show (Constant _) = "Constant"
    show (Arr _) = "Arr"
    show (Pre _) = "Pre"
    show (f :>>>: g) = "(" ++ show f ++ ") >>> (" ++ show g ++ ")"
    show (f :***: g) = show f ++ " *** " ++ show g
    show App = "App"
    show (Loop f) = "Loop " ++ show f
    show (f :+++: g) = "(" ++ show f ++ ") +++ (" ++ show g ++ ")"

-- Bringing the constructors into scope to override the Control.Arrow constructors.
-- This might eventually be overloaded with a class.

id :: GenArrow ArrowId a a
id = Id

(>>>) :: GenArrow ar a b -> GenArrow ar' b c -> GenArrow (ArrowGGG ar ar' b) a c
f >>> g = f :>>>: g

(***) :: GenArrow ar a b -> GenArrow ar' a' b' -> GenArrow (ArrowSSS ar ar') (P a a') (P b b')
f *** g = f :***: g

dup :: GenArrow ArrowDup a (P a a)
dup = Dup

dropL :: GenArrow ArrowDropL (P a b) b
dropL = DropL

dropR :: GenArrow ArrowDropR (P a b) a
dropR = DropR

constant :: Val a -> GenArrow ArrowConst x a
constant = Constant

first :: GenArrow ar a b -> GenArrow (ArrowSSS ar ArrowId) (P a c) (P b c)
first = (*** id)

second :: GenArrow ar a b -> GenArrow (ArrowSSS ArrowId ar) (P c a) (P c b)
second = (id ***)

arr :: (Val a -> Val b) -> GenArrow ArrowArr a b
arr = Arr

-- A few helpers for arr:
arr11 :: (a -> b) -> GenArrow ArrowArr (V a) (V b)
arr11 f = arr $ \(One x) -> One (f x)
arr21 :: (a -> b -> c) -> GenArrow ArrowArr (P (V a) (V b)) (V c)
arr21 f = arr $ \(Pair (One x) (One y)) -> One (f x y)

app :: GenArrow ArrowApp (P (V (GenArrow ar b c)) b) c
app = App

loop :: GenArrow ar (P a c) (P b c) -> GenArrow (ArrowLoop ar c) a b
loop = Loop

(+++) :: GenArrow ar a b -> GenArrow ar' c d -> GenArrow (ArrowPPP ar ar') (C a c) (C b d)
f +++ g = f :+++: g

(|||) :: GenArrow ar a b -> GenArrow ar' c b -> GenArrow (ArrowGGG (ArrowPPP ar ar') ArrowArr (C b b)) (C a c) b
f ||| g = f +++ g >>> arr untag
    where
        untag :: Val (C b b) -> Val b
        untag (Choice1 v) = v
        untag (Choice2 v) = v

pre :: Val a -> GenArrow ArrowPre a a
pre = Pre

-- A helper for pre:
pre1 :: a -> GenArrow ArrowPre (V a) (V a)
pre1 a = Pre (One a)

returnA :: GenArrow ArrowId a a
returnA = Id

-- Running functions for testing purposes.

runGenArrow :: GenArrow ar a b -> [Val a] -> [Val b]
runGenArrow _ [] = []
runGenArrow ga (x : xs) = let
    (y, ga') = run ga x
    in y : runGenArrow ga' xs
    where
        run :: GenArrow ar a b -> Val a -> (Val b, GenArrow ar a b)
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