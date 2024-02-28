module AFRP where

import Rearrange
import GHC.TypeLits
import Data.IORef (writeIORef, readIORef)
import Data.Proxy
import Data.Kind (Type)
import Prelude hiding (id)

data SKind where
    O :: SKind
    (:::) :: SKind -> SKind -> SKind

type Desc :: SKind -> Type
data Desc x where
    V :: forall (a :: Type). a -> Desc O
    P :: Desc a -> Desc b -> Desc (a ::: b)

type Val :: forall s. Desc s -> Type
data Val x where
    One :: a -> Val (V a)
    Pair :: Val a -> Val b -> Val (P a b)

instance Show a => Show (Val (V a)) where
    show (One a) = show a

instance (Show (Val l), Show (Val r)) => Show (Val (P l r)) where
    show (Pair l r) = "[|" ++ show l ++ ", " ++ show r ++ "|]"

type Con :: SKind -> SKind -> Type
data Con ar ar' where
    IdCon :: Con a a
    DropLCon :: Con (a ::: b) b
    DropRCon :: Con (a ::: b) a
    DupCon :: Con a (a ::: a)
    ConstCon :: Con a b

    ArrCon :: Con a b
    PreCon :: Con a a
    GGGCon :: Con a b -> Con b c -> Desc b -> Con a c
    SSSCon :: Con a b -> Con a' b' -> Con (a ::: a') (b ::: b')
    LoopCon :: Con (a ::: c) (b ::: c) -> Desc c -> Con a b

-- Programmers will likely use :: SF _ a b in their code, since _ is entirely inferrable from the constructors.
type SF :: forall (sk :: SKind) (sk' :: SKind).
    Con sk sk' -> Desc sk -> Desc sk' -> Type
data SF arrow a b where
    -- Routing
    Id :: SF IdCon a a
    DropL :: SF DropLCon (P a b) b
    DropR :: SF DropRCon (P a b) a
    Dup :: SF DupCon a (P a a)
    Constant :: Val a -> SF ConstCon x a
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr :: (Val a -> Val b) -> SF ArrCon a b
    Pre :: Val a -> SF PreCon a a
    (:>>>:) :: SF ar a b -> SF ar' b c -> SF (GGGCon ar ar' b) a c
    (:***:) :: SF ar a b -> SF ar' a' b' -> SF (SSSCon ar ar') (P a a') (P b b')
    Loop :: SF ar (P a c) (P b c) -> SF (LoopCon ar c) a b

instance Show (SF ar a b) where
    show Id = "Id"
    show DropL = "DropL"
    show DropR = "DropR"
    show Dup = "Dup"
    show (Constant _) = "Constant"
    show (Arr _) = "Arr"
    show (Pre _) = "Pre"
    show (f :>>>: g) = "(" ++ show f ++ ") >>> (" ++ show g ++ ")"
    show (f :***: g) = show f ++ " *** " ++ show g
    show (Loop f) = "Loop " ++ show f

-- Bringing the constructors into scope to override the Control.Arrow constructors.
-- This might eventually be overloaded with a class.

id :: SF IdCon a a
id = Id

(>>>) :: SF ar a b -> SF ar' b c -> SF (GGGCon ar ar' b) a c
f >>> g = f :>>>: g

(***) :: SF ar a b -> SF ar' a' b' -> SF (SSSCon ar ar') (P a a') (P b b')
f *** g = f :***: g

dup :: SF DupCon a (P a a)
dup = Dup

dropL :: SF DropLCon (P a b) b
dropL = DropL

dropR :: SF DropRCon (P a b) a
dropR = DropR

constant :: Val a -> SF ConstCon x a
constant = Constant

first :: SF ar a b -> SF (SSSCon ar IdCon) (P a c) (P b c)
first = (*** id)

second :: SF ar a b -> SF (SSSCon IdCon ar) (P c a) (P c b)
second = (id ***)

arr :: (Val a -> Val b) -> SF ArrCon a b
arr = Arr

-- A few helpers for arr:
arr11 :: (a -> b) -> SF ArrCon (V a) (V b)
arr11 f = arr $ \(One x) -> One (f x)
arr21 :: (a -> b -> c) -> SF ArrCon (P (V a) (V b)) (V c)
arr21 f = arr $ \(Pair (One x) (One y)) -> One (f x y)

loop :: SF ar (P a c) (P b c) -> SF (LoopCon ar c) a b
loop = Loop

pre :: Val a -> SF PreCon a a
pre = Pre

-- A helper for pre:
pre1 :: a -> SF PreCon (V a) (V a)
pre1 a = Pre (One a)

returnA :: SF IdCon a a
returnA = Id

-- Running functions for testing purposes.

runSF :: SF ar a b -> [Val a] -> [Val b]
runSF _ [] = []
runSF ga (x : xs) = let
    (y, ga') = run ga x
    in y : runSF ga' xs
    where
        run :: SF ar a b -> Val a -> (Val b, SF ar a b)
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
        run (Loop f) a =
            let (Pair b c, f') = run f (Pair a c) in (b, Loop f')

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

type ConAnn :: SKind -> SKind -> *
data ConAnn ar ar' where
    IdCon' :: ConAnn a a
    DropLCon' :: ConAnn (a ::: b) b
    DropRCon' :: ConAnn (a ::: b) a
    DupCon' :: ConAnn a (a ::: a)
    ConstCon' :: DescAnn b -> ConAnn a b
    ArrCon' :: ConAnn a b
    PreCon' :: ConAnn a a
    GGGCon' :: ConAnn a b -> ConAnn b c -> DescAnn b -> ConAnn a c
    SSSCon' :: ConAnn a b -> ConAnn a' b' -> ConAnn (a ::: a') (b ::: b')
    LoopCon' :: ConAnn (a ::: c) (b ::: c) -> DescAnn c -> ConAnn a b

type SFAnn :: forall (sk :: SKind) (sk' :: SKind).
    ConAnn sk sk' -> DescAnn sk -> DescAnn sk' -> *
data SFAnn conAnn a b where
    -- Routing
    Id' :: SFAnn IdCon' a a
    DropL' :: SFAnn DropLCon' (PN a b) b
    DropR' :: SFAnn DropRCon' (PN a b) a
    Dup' :: SFAnn DupCon' a (PN a a)
    Constant' :: Val (AsDesc a) -> SFAnn (ConstCon' a) x a
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr' :: (Val (AsDesc a) -> Val (AsDesc b)) -> SFAnn ArrCon' a b
    Pre' :: Val (AsDesc a') -> SFAnn PreCon' a a'
    (:>>>::) :: SFAnn ar a b -> SFAnn ar' b c -> SFAnn (GGGCon' ar ar' b) a c
    (:***::) :: SFAnn ar a b -> SFAnn ar' a' b' -> SFAnn (SSSCon' ar ar') (PN a a') (PN b b')
    Loop' :: SFAnn ar (PN a c) (PN b c) -> SFAnn (LoopCon' ar c) a b
