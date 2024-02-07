-- This module implements the common HList structure, along with a few helper
-- functions.

{-# LANGUAGE FlexibleInstances, UndecidableInstances, FunctionalDependencies,
    ScopedTypeVariables #-}

module Data.Type.HList (
    HList(..),
    hCombine, Combine,
    hHead, hTail, hAppend, Append,
    FlattenToHList,
    LookupNth(..), hReverse, HReverse,
    ) where

import Data.Type.Utils (Combine)
import Data.Proxy
import GHC.TypeLits

-- The HList structure.

data HList :: [*] -> * where
    HNil :: HList '[]
    (:+:) :: x -> HList xs -> HList (x ': xs)

infixr 5 :+:

-- Useful homogeneous list functions ported to HLists.

instance Show (HList '[]) where
    show HNil = "HNil"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
    show (x :+: xs) = "(" ++ show x ++ ") : " ++ show xs

hCombine :: HList xs -> HList ys -> HList (Combine xs ys)
hCombine HNil ys = ys
hCombine (x :+: xs) ys = x :+: hCombine xs ys

hHead :: HList (x ': xs) -> x
hHead (x :+: _) = x

hTail :: HList (x ': xs) -> HList xs
hTail (_ :+: xs) = xs

type family Append x xs where
    Append y '[] = '[y]
    Append y (x ': xs) = x ': Append y xs

hAppend :: x -> HList xs -> HList (Append x xs)
hAppend y HNil = y :+: HNil
hAppend y (x :+: xs) = x :+: hAppend y xs

-- FlattenToHList, which removes a layer of nesting by using HLists.

type family FlattenToHList (inp :: [[*]]) :: [*] where
    FlattenToHList '[] = '[]
    FlattenToHList (x ': xs) = HList x ': FlattenToHList xs

class LookupNth n xs r | n xs -> r where
    lookupNth :: Proxy n -> HList xs -> r

instance {-# OVERLAPPING #-} LookupNth 0 (x ': xs) x where
    lookupNth Proxy (v :+: _) = v

instance {-# OVERLAPPABLE #-}
    forall n x xs r. (LookupNth (n-1) xs r) => LookupNth n (x ': xs) r where
    lookupNth Proxy (_ :+: vs) = lookupNth (Proxy :: Proxy (n-1)) vs

type HReverse xs xs' = HReverse' xs '[] xs'

hReverse :: HReverse xs xs' => HList xs -> HList xs'
hReverse xs = hReverse' xs HNil

class HReverse' xs acc xs' | xs acc -> xs' where
    hReverse' :: HList xs -> HList acc -> HList xs'

instance HReverse' '[] acc acc where
    hReverse' HNil acc = acc

instance (HReverse' xs (x ': acc) xs') => HReverse' (x ': xs) acc xs' where
    hReverse' (x :+: xs) acc = hReverse' xs (x :+: acc)