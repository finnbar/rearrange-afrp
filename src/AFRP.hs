module AFRP (AFRP, module GenProc.GeneralisedArrow, AFRPCon,
    AFRPAnn(..), AFRPConAnn(..), DescAnn(..), Ref(..), AsDesc,
    readRef, writeRef, splitProx, pairProx) where

import Rearrange
import GHC.TypeLits
import Data.IORef (writeIORef, readIORef)
import Data.Proxy

import GenProc.GeneralisedArrow (Desc(..), Arrow(..), Val(..), SKind(..), GenArrow(..))

-- The arrow combinators are taken from our generalised proc notation.
type AFRP = GenArrow
type AFRPCon = Arrow

type DescAnn :: SKind -> *
data DescAnn ar where
    VN :: forall (a :: *). Nat -> a -> DescAnn O
    PN :: DescAnn l -> DescAnn r -> DescAnn (l ::: r)

type Ref :: forall s. DescAnn s -> *
data Ref desc where
    VRef :: IOCell n a -> Ref (VN n a)
    PRef :: Ref l -> Ref r -> Ref (PN l r)

type AsDesc :: forall (ar :: SKind). DescAnn ar -> Desc ar
type family AsDesc d' where
    AsDesc (VN n a) = V a
    AsDesc (PN l r) = P (AsDesc l) (AsDesc r)

readRef :: Ref d -> IO (Val (AsDesc d))
readRef (VRef (Cell i)) = One <$> readIORef i
readRef (PRef l r) = Pair <$> readRef l <*> readRef r

writeRef :: Ref d -> Val (AsDesc d) -> IO ()
writeRef (VRef (Cell i)) (One v) = writeIORef i v
writeRef (PRef l r) (Pair i j) = writeRef l i Prelude.>> writeRef r j

splitProx :: Proxy (d l r) -> (Proxy l, Proxy r)
splitProx Proxy = (Proxy, Proxy)
pairProx :: Proxy l -> Proxy r -> Proxy (d l r)
pairProx Proxy Proxy = Proxy

-- Programmers write their code in this intermediate GADT.
-- This is so that programmers can write programs without worrying about names.
-- This should allow full inference: assume we have a well-typed AFRP, then we transform to well-typed RAFRP,
-- which can then be (possibly) implemented with rearrange.

type AFRPConAnn :: SKind -> SKind -> *
data AFRPConAnn ar ar' where
    ArrowId' :: AFRPConAnn a a
    ArrowDropL' :: AFRPConAnn (a ::: b) b
    ArrowDropR' :: AFRPConAnn (a ::: b) a
    ArrowDup' :: AFRPConAnn a (a ::: a)
    ArrowConst' :: DescAnn b -> AFRPConAnn a b
    ArrowArr' :: AFRPConAnn a b
    ArrowPre' :: AFRPConAnn a a
    ArrowGGG' :: AFRPConAnn a b -> AFRPConAnn b c -> DescAnn b -> AFRPConAnn a c
    ArrowSSS' :: AFRPConAnn a b -> AFRPConAnn a' b' -> AFRPConAnn (a ::: a') (b ::: b')
    ArrowLoop' :: AFRPConAnn (a ::: c) (b ::: c) -> DescAnn c -> AFRPConAnn a b

type AFRPAnn :: forall (ar :: SKind) (ar' :: SKind).
    AFRPConAnn ar ar' -> DescAnn ar -> DescAnn ar' -> *
data AFRPAnn arrow a b where
    -- Routing
    Id' :: AFRPAnn ArrowId' a a
    DropL' :: AFRPAnn ArrowDropL' (PN a b) b
    DropR' :: AFRPAnn ArrowDropR' (PN a b) a
    Dup' :: AFRPAnn ArrowDup' a (PN a a)
    Constant' :: Val (AsDesc a) -> AFRPAnn (ArrowConst' a) x a
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr' :: (Val (AsDesc a) -> Val (AsDesc b)) -> AFRPAnn ArrowArr' a b
    Pre' :: Val (AsDesc a') -> AFRPAnn ArrowPre' a a'
    (:>>>::) :: AFRPAnn ar a b -> AFRPAnn ar' b c -> AFRPAnn (ArrowGGG' ar ar' b) a c
    (:***::) :: AFRPAnn ar a b -> AFRPAnn ar' a' b' -> AFRPAnn (ArrowSSS' ar ar') (PN a a') (PN b b')
    Loop' :: AFRPAnn ar (PN a c) (PN b c) -> AFRPAnn (ArrowLoop' ar c) a b
