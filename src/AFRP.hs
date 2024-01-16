module AFRP (GenArrow(..), AFRP, Arrow(..), Desc(..), Val(..), Arity(..),
    AFRP'(..), Arrow'(..), Desc'(..), Ref(..), AsDesc,
    readRef, writeRef, splitProx, pairProx) where

import Rearrange
import GHC.TypeLits
import Data.IORef (writeIORef, readIORef)
import Data.Proxy

import GenProc.GeneralisedArrow (Desc(..), Arrow(..), Val(..), Arity(..), GenArrow(..))

type AFRP = GenArrow

type Desc' :: Arity -> *
data Desc' ar where
    VN :: forall (a :: *). Nat -> a -> Desc' O
    PN :: Desc' l -> Desc' r -> Desc' (l ::: r)

type Ref :: forall s. Desc' s -> *
data Ref desc where
    VRef :: IOCell n a -> Ref (VN n a)
    PRef :: Ref l -> Ref r -> Ref (PN l r)

type AsDesc :: forall (ar :: Arity). Desc' ar -> Desc ar
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

type Arrow' :: Arity -> Arity -> *
data Arrow' ar ar' where
    ArrowId' :: Arrow' a a
    ArrowDropL' :: Arrow' (a ::: b) b
    ArrowDropR' :: Arrow' (a ::: b) a
    ArrowDup' :: Arrow' a (a ::: a)
    ArrowArr' :: Arrow' a b
    ArrowPre' :: Arrow' a a
    ArrowGGG' :: Arrow' a b -> Arrow' b c -> Desc' b -> Arrow' a c
    ArrowSSS' :: Arrow' a b -> Arrow' a' b' -> Arrow' (a ::: a') (b ::: b')
    ArrowLoop' :: Arrow' (a ::: c) (b ::: c) -> Desc' c -> Arrow' a b

type AFRP' :: forall (ar :: Arity) (ar' :: Arity).
    Arrow' ar ar' -> Desc' ar -> Desc' ar' -> *
data AFRP' arrow a b where
    -- Routing
    Id' :: AFRP' ArrowId' a a
    DropL' :: AFRP' ArrowDropL' (PN a b) b
    DropR' :: AFRP' ArrowDropR' (PN a b) a
    Dup' :: AFRP' ArrowDup' a (PN a a)
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr' :: (Val (AsDesc a) -> Val (AsDesc b)) -> AFRP' ArrowArr' a b
    Pre' :: Val (AsDesc a') -> AFRP' ArrowPre' a a'
    (:>>>::) :: AFRP' ar a b -> AFRP' ar' b c -> AFRP' (ArrowGGG' ar ar' b) a c
    (:***::) :: AFRP' ar a b -> AFRP' ar' a' b' -> AFRP' (ArrowSSS' ar ar') (PN a a') (PN b b')
    Loop' :: AFRP' ar (PN a c) (PN b c) -> AFRP' (ArrowLoop' ar c) a b
