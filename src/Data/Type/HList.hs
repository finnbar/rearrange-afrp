-- This module implements the common HList structure, along with a few helper
-- functions.

{-# LANGUAGE FlexibleInstances, UndecidableInstances, FunctionalDependencies,
    ScopedTypeVariables #-}

module Data.Type.HList (
    HList(..),
    hCombine, hHead, hTail,
    FlattenToHList,
    LookupNth(..)
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

-- FlattenToHList, which removes a layer of nesting by using HLists.

type family FlattenToHList (inp :: [[*]]) :: [*] where
    FlattenToHList '[] = '[]
    FlattenToHList (x ': xs) = HList x ': FlattenToHList xs

class LookupNth n xs r | n xs -> r where
    lookupNth :: Proxy n -> HList xs -> r

instance LookupNth 0 (x ': xs) x where
    lookupNth Proxy (v :+: _) = v

instance forall n x xs r. (LookupNth (n-1) xs r) => LookupNth n (x ': xs) r where
    lookupNth Proxy (_ :+: vs) = lookupNth (Proxy :: Proxy (n-1)) vs