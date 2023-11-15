module AFRP where

import Rearrange
import GHC.TypeLits
import Data.IORef (writeIORef, readIORef)
import Data.Proxy

data Arity where
    O :: Arity
    (:::) :: Arity -> Arity -> Arity

type Desc :: Arity -> *
data Desc x where
    V :: forall (a :: *). a -> Desc O
    P :: Desc a -> Desc b -> Desc (a ::: b)

type Desc' :: Arity -> *
data Desc' ar where
    VN :: forall (a :: *). Nat -> a -> Desc' O
    PN :: Desc' l -> Desc' r -> Desc' (l ::: r)

type Val :: forall s. Desc s -> *
data Val x where
    One :: !a -> Val (V a)
    Pair :: !(Val a) -> !(Val b) -> Val (P a b)

instance Show a => Show (Val (V a)) where
    show (One a) = show a

instance (Show (Val l), Show (Val r)) => Show (Val (P l r)) where
    show (Pair l r) = "[|" ++ show l ++ ", " ++ show r ++ "|]"

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

type Arrow :: (Arity -> *) -> Arity -> Arity -> *
data Arrow desc ar ar' where
    ArrowId :: Arrow desc a a
    ArrowDropL :: Arrow desc (a ::: b) b
    ArrowDropR :: Arrow desc (a ::: b) a
    ArrowDup :: Arrow desc a (a ::: a)
    ArrowArr :: Arrow desc a b
    ArrowPre :: Arrow desc a a
    ArrowGGG :: Arrow desc a b -> Arrow desc b c -> desc b -> Arrow desc a c
    ArrowSSS :: Arrow desc a b -> Arrow desc a' b' -> Arrow desc (a ::: a') (b ::: b')
    ArrowLoop :: Arrow desc (a ::: c) (b ::: c) -> desc c -> Arrow desc a b

-- Programmers will likely use :: AFRP _ a b in their code, since _ is entirely inferrable from the constructors.
-- We need it to implement an AFRP -> RAFRP function, as the type of the output cannot solely depend on the value of the input.
-- (That's dependent types innit.)
type AFRP :: forall (ar :: Arity) (ar' :: Arity).
    Arrow Desc ar ar' -> Desc ar -> Desc ar' -> *
data AFRP arrow a b where
    -- Routing
    Id :: AFRP ArrowId a a
    DropL :: AFRP ArrowDropL (P a b) b
    DropR :: AFRP ArrowDropR (P a b) a
    Dup :: AFRP ArrowDup a (P a a)
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr :: (Val a -> Val b) -> AFRP ArrowArr a b
    Pre :: Val a -> AFRP ArrowPre a a
    (:>>>:) :: AFRP ar a b -> AFRP ar' b c -> AFRP (ArrowGGG ar ar' b) a c
    (:***:) :: AFRP ar a b -> AFRP ar' a' b' -> AFRP (ArrowSSS ar ar') (P a a') (P b b')
    Loop :: AFRP ar (P a c) (P b c) -> AFRP (ArrowLoop ar c) a b

type AFRP' :: forall (ar :: Arity) (ar' :: Arity).
    Arrow Desc' ar ar' -> Desc' ar -> Desc' ar' -> *
data AFRP' arrow a b where
    -- Routing
    Id' :: AFRP' ArrowId a a
    DropL' :: AFRP' ArrowDropL (PN a b) b
    DropR' :: AFRP' ArrowDropR (PN a b) a
    Dup' :: AFRP' ArrowDup a (PN a a)
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr' :: (Val (AsDesc a) -> Val (AsDesc b)) -> AFRP' ArrowArr a b
    Pre' :: Val (AsDesc a') -> AFRP' ArrowPre a a'
    (:>>>>:) :: AFRP' ar a b -> AFRP' ar' b c -> AFRP' (ArrowGGG ar ar' b) a c
    (:****:) :: AFRP' ar a b -> AFRP' ar' a' b' -> AFRP' (ArrowSSS ar ar') (PN a a') (PN b b')
    Loop' :: AFRP' ar (PN a c) (PN b c) -> AFRP' (ArrowLoop ar c) a b
