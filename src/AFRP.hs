{-# LANGUAGE FunctionalDependencies, UndecidableInstances, QualifiedDo,
        AllowAmbiguousTypes #-}

module AFRP where

import Rearrange
import Data.Memory.Types (MemState, MemoryPlus)
import Data.Memory.Memory (MemoryInv)
import Data.Type.Equality
import Data.Type.HList
import GHC.TypeLits
import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import Data.Proxy
import Data.Kind (Type, Constraint)
import Data.Type.Bool (If)

data Arity where
    O :: Arity
    (:::) :: Arity -> Arity -> Arity
    (:?:) :: Arity -> Arity -> Arity

type Desc :: Arity -> Type
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

type Arrow :: Arity -> Arity -> Type
data Arrow ar ar' where
    ArrowId :: Arrow a a
    ArrowDropL :: Arrow (a ::: b) b
    ArrowDropR :: Arrow (a ::: b) a
    ArrowDup :: Arrow a (a ::: a)
    ArrowConst :: Desc b -> Arrow a b

    ArrowArr :: Desc b -> Arrow a b
    ArrowPre :: Arrow a a
    ArrowGGG :: Arrow a b -> Arrow b c -> Arrow a c
    ArrowSSS :: Arrow a b -> Arrow a' b' -> Arrow (a ::: a') (b ::: b')
    ArrowApp :: Arrow (O ::: a) b
    ArrowLoop :: Arrow (a ::: c) (b ::: c) -> Desc c -> Arrow a b
    ArrowPPP :: Arrow a b -> Arrow c d -> Arrow (a :?: c) (b :?: d)

-- Programmers will likely use :: AFRP _ a b in their code, since _ is entirely inferrable from the constructors.
-- We need it to implement an AFRP -> RAFRP function, as the type of the output cannot solely depend on the value of the input.
-- (That's dependent types innit.)
type AFRP :: forall (ar :: Arity) (ar' :: Arity).
    Arrow ar ar' -> Desc ar -> Desc ar' -> Type
data AFRP arrow a b where
    -- Routing
    Id :: AFRP ArrowId a a
    DropL :: AFRP ArrowDropL (P a b) b
    DropR :: AFRP ArrowDropR (P a b) a
    Dup :: AFRP ArrowDup a (P a a)
    Constant :: Val a -> AFRP (ArrowConst a) x a
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr :: (Val a -> Val b) -> AFRP (ArrowArr b) a b
    Pre :: Val a -> AFRP ArrowPre a a
    (:>>>:) :: (AssignMemory arrl a b arrl' a' b' fs fs',
        AssignMemory arrr b c arrr' b' c' fs' fs'') =>
        AFRP arrl a b -> AFRP arrr b c -> AFRP (ArrowGGG arrl arrr) a c
    (:***:) :: AFRP ar a b -> AFRP ar' a' b' -> AFRP (ArrowSSS ar ar') (P a a') (P b b')
    App :: AFRP ArrowApp (P (V (AFRP ar b c)) b) c
    Loop :: AFRP ar (P a c) (P b c) -> AFRP (ArrowLoop ar c) a b
    (:+++:) :: AFRP ar a b -> AFRP ar' c d -> AFRP (ArrowPPP ar ar') (C a c) (C b d)

instance Show (AFRP ar a b) where
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

-- AFRP' SETUP

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

type Arrow' :: Arity -> Arity -> *
data Arrow' ar ar' where
    ArrowId' :: Arrow' a a
    ArrowDropL' :: Arrow' (a ::: b) b
    ArrowDropR' :: Arrow' (a ::: b) a
    ArrowDup' :: Arrow' a (a ::: a)
    ArrowConst' :: Desc' b -> Arrow' a b
    ArrowArr' :: Desc' b -> Arrow' a b
    ArrowPre' :: Desc' a -> Arrow' a a
    ArrowGGG' :: Arrow' a b -> Arrow' b c -> Arrow' a c
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
    Constant' :: Val (AsDesc a) -> AFRP' (ArrowConst' a) x a
    -- NB Swap = Dup >>> (DropL *** DropR)
    -- Assoc = Dup >>> ((Id *** DropR) *** (DropL >>> DropL))
    -- Unassoc = Dup >>> ((DropR >>> DropR) *** (DropL *** Id))
    -- Thus we only need Drop and Dup.

    -- Arrows
    Arr' :: (Val (AsDesc a) -> Val (AsDesc b)) -> AFRP' (ArrowArr' b) a b
    Pre' :: Val (AsDesc a') -> AFRP' (ArrowPre' a') a a'
    (:>>>::) :: AFRP' ar a b -> AFRP' ar' b c -> AFRP' (ArrowGGG' ar ar') a c
    (:***::) :: AFRP' ar a b -> AFRP' ar' a' b' -> AFRP' (ArrowSSS' ar ar') (PN a a') (PN b b')
    Loop' :: AFRP' ar (PN a c) (PN b c) -> AFRP' (ArrowLoop' ar c) a b

-- NAMING PROCESS: Converting AFRP to AFRP'

-- Have the ability to make Fresh names to fill in the gaps in some Desc'.

type FreshState = (Nat, [*])
type BuildState :: FreshState -> *
type family EnvFromBuildState (x :: FreshState) where
    EnvFromBuildState '(n, env) = env
newtype BuildState (fs :: FreshState) = MkBuildState (HList (EnvFromBuildState fs))

type Fresh :: forall s. Desc s -> FreshState -> Desc' s -> FreshState -> Constraint
class Fresh d fs d' fs' | d fs -> d' fs' where
    -- Create a fresh undefined variable.
    fresh :: BuildState fs -> Proxy d
        -> IO (Proxy d', BuildState fs')

instance (n' ~ n + 1, env' ~ Append (Rearrange.IOCell n a) env) =>
    Fresh (V a) '(n, env) (VN n a) '(n', env') where
    fresh (MkBuildState ps) Proxy = do
        ref <- newIORef undefined
        let cell = Rearrange.Cell @n @a @IO @IORef ref
        Prelude.return (Proxy, MkBuildState $ hAppend @(Rearrange.IOCell n a) cell ps)

instance (Fresh l fs lns fs', Fresh r fs' rns fs'') =>
    Fresh (P l r) fs (PN lns rns) fs'' where
    fresh bs prox = do
        let (lp, rp) = splitProx prox
        (lp', bs') <- fresh bs lp
        (rp', bs'') <- fresh bs' rp
        Prelude.return (pairProx lp' rp', bs'')

type EmptyFreshState = '(0, '[])
newBuildState :: BuildState EmptyFreshState
newBuildState = MkBuildState HNil

-- The challenge with naming is that loops create cycles.
-- We therefore assignMemory as normal with various names, and then unify those that should
-- be the same with Substitute.
type AssignMemory :: Arrow ar ar' -> Desc ar -> Desc ar'
    -> Arrow' ar ar' -> Desc' ar -> Desc' ar'
    -> FreshState -> FreshState -> Constraint
class AssignMemory arr a b arr' a' b' fs fs' | arr a a' fs -> arr' b b' fs' where
    assignMemory :: AFRP arr a b -> Proxy a' -> BuildState fs ->
        IO (AFRP' arr' a' b', Proxy b', BuildState fs')

instance AssignMemory ArrowId a a ArrowId' a' a' fs fs where
    assignMemory Id prox bs = Prelude.return (Id', prox, bs)

instance AssignMemory ArrowDropL (P a b) b ArrowDropL' (PN a' b') b' fs fs where
    assignMemory DropL prox bs = let (_, r) = splitProx prox in Prelude.return (DropL', r, bs)

instance AssignMemory ArrowDropR (P a b) a ArrowDropR' (PN a' b') a' fs fs where
    assignMemory DropR prox bs = let (l, _) = splitProx prox in Prelude.return (DropR', l, bs)

instance AssignMemory ArrowDup a (P a a) ArrowDup' a' (PN a' a') fs fs where
    assignMemory Dup prox bs = Prelude.return (Dup', pairProx prox prox, bs)

instance (Fresh a fs a' fs', x ~ AsDesc x', a ~ AsDesc a') =>
    AssignMemory (ArrowConst a) x a (ArrowConst' a') x' a' fs fs' where
    assignMemory (Constant x) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy a)
        Prelude.return (Constant' x, prox', bs')

instance (Fresh b fs b' fs', a ~ AsDesc a', b ~ AsDesc b') =>
    AssignMemory (ArrowArr b) a b (ArrowArr' b') a' b' fs fs' where
    assignMemory (Arr f) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy b)
        Prelude.return (Arr' f, prox', bs')

instance (Fresh a fs b' fs', a ~ AsDesc a', a ~ AsDesc b') =>
    AssignMemory ArrowPre a a (ArrowPre' b') a' b' fs fs' where
    assignMemory (Pre v) _ bs = do
        (prox', bs') <- fresh bs (Proxy :: Proxy a)
        Prelude.return (Pre' v, prox', bs')

instance AssignMemory (ArrowGGG arrl arrr) a c (ArrowGGG' arrl' arrr') a' c' fs fs'' where
    assignMemory (f :>>>: g) prox bs = do
        (f', prox', bs') <- assignMemory f prox bs
        (g', prox'', bs'') <- assignMemory g prox' bs'
        Prelude.return (f' :>>>:: g', prox'', bs'')

instance (AssignMemory arrl a c arrl' a' c' fs fs',
    AssignMemory arrr b d arrr' b' d' fs' fs'') =>
    AssignMemory (ArrowSSS arrl arrr) (P a b) (P c d)
    (ArrowSSS' arrl' arrr') (PN a' b') (PN c' d') fs fs'' where
    assignMemory (f :***: g) prox bs = do
        let (lp, rp) = splitProx prox
        (f', lp', bs') <- assignMemory f lp bs
        (g', rp', bs'') <- assignMemory g rp bs'
        Prelude.return (f' :***:: g', pairProx lp' rp', bs'')

-- For loops:
-- Make Fresh variables for the second input, use them to get the second output and then unify.
instance (Fresh c fs c' fs',
    -- AssignMemory the body once with the updated input Desc'
    AssignMemory arr (P a c) (P b c) arr' (PN a' c') (PN b' d') fs' fs'',
    -- Then repeatedly substitute until the looped desc is constant.
    RepeatSub arr' a' b' c' d' arr'' b'' c'' (c' == d')) =>
    AssignMemory (ArrowLoop arr c) a b (ArrowLoop' arr'' c'') a' b'' fs fs'' where
    assignMemory (Loop f) prox bs = do
        (proxc, bs') <- fresh bs (Proxy :: Proxy c)
        (f', Proxy, bs'') <- assignMemory f (pairProx prox proxc) bs'
        let f'' = repeatSub (Proxy :: Proxy (c' == d')) f'
        Prelude.return (Loop' f'', Proxy :: Proxy b'', bs'')

class RepeatSub arr a b c d arr' b' c' eq | arr a b c d eq -> arr' b' c' where
    repeatSub :: Proxy eq -> AFRP' arr (PN a c) (PN b d) -> AFRP' arr' (PN a c') (PN b' c')

-- If we already have something of the right form, we are done.
instance RepeatSub arr a b c c arr b c 'True where
    repeatSub _ afrp = afrp

-- If not, we must go again.
instance (sub ~ ToSubstitution c d,
    Substitute arr (PN a c) (PN b d) arr' (PN a c') (PN b' d') sub,
    -- Then recursively call:
    RepeatSub arr' a b' c' d' arr'' b'' c'' (c' == d')) =>
    RepeatSub arr a b c d arr'' b'' c'' 'False where
    
    repeatSub Proxy afrp =
        repeatSub (Proxy :: Proxy (c' == d')) (substitute afrp (Proxy :: Proxy sub))

-- Within x, substitutes all Nat in s' with their equivalent in s.
-- INVARIANT: SingleSub s s' => s < s'
-- NOTE There is a subtle requirement here: if we were instead to swap the condition, we could
-- accidentally overwrite variables from outside of the loop. We utilise the fact that the first
-- input of the loop will have a lower label n than the second input n' -- so if the second output
-- contains n then it won't be overwritten with n'. 
-- We can theoretically avoid this by performing the substitution over the entire program,
-- but this sounds like a typing nightmare due to the requirement on Loop' that the second input/
-- output agree.
-- Simple explanation: by substituting large for small, we guarantee to only be touching names
-- generated within the loop.
data Substitution = Sub Nat Nat

type ToSubstitution :: Desc' d -> Desc' d -> [Substitution]
type family ToSubstitution d d' where
    ToSubstitution (PN l r) (PN l' r') = Combine (ToSubstitution l l') (ToSubstitution r r')
    ToSubstitution (VN n a) (VN n' a) = If (n <=? n') '[Sub n n'] '[Sub n' n]

type ApplySub :: Desc' a -> Substitution -> Desc' a
type family ApplySub n sub where
    ApplySub (PN l r) sub = PN (ApplySub l sub) (ApplySub r sub)
    ApplySub (VN n a) (Sub n' n) = VN n' a
    ApplySub (VN n a) (Sub x y)  = VN n a

type Subst :: Desc' a -> [Substitution] -> Desc' a
type family Subst x sub where
    Subst desc (sub : xs) = Subst (ApplySub desc sub) xs
    Subst desc '[] = desc

class Substitute arr a b arr' a' b' sub | arr a sub -> b arr' a' b' where
    substitute :: AFRP' arr a b -> Proxy sub -> AFRP' arr' a' b'

instance (a' ~ Subst a sub) => Substitute ArrowId' a a ArrowId' a' a' sub where
    substitute Id' _ = Id'

instance (a' ~ Subst a sub, b' ~ Subst b sub) =>
    Substitute ArrowDropL' (PN a b) b ArrowDropL' (PN a' b') b' sub where
    substitute DropL' _ = DropL'

instance (a' ~ Subst a sub, b' ~ Subst b sub) =>
    Substitute ArrowDropR' (PN a b) a ArrowDropR' (PN a' b') a' sub where
    substitute DropR' _ = DropR'

instance (a' ~ Subst a sub) =>
    Substitute ArrowDup' a (PN a a) ArrowDup' a' (PN a' a') sub where
    substitute Dup' _ = Dup'

instance (a' ~ Subst a sub, b' ~ Subst b sub, AsDesc a ~ AsDesc a', AsDesc b ~ AsDesc b') =>
    Substitute (ArrowArr' b) a b (ArrowArr' b') a' b' sub where
    substitute (Arr' f) _ = Arr' f

instance (b ~ Subst a sub, b' ~ Subst a' sub, AsDesc a ~ AsDesc b, AsDesc a' ~ AsDesc b') =>
    Substitute (ArrowPre' a') a a' (ArrowPre' b') b b' sub where
    substitute (Pre' v) _ = Pre' v

instance (Substitute arrl a b arrl' a' b' sub, Substitute arrr b c arrr' b' c' sub) =>
    Substitute (ArrowGGG' arrl arrr) a c (ArrowGGG' arrl' arrr') a' c' sub where
    substitute (f :>>>:: g) sub = substitute f sub :>>>:: substitute g sub

instance (Substitute arrl a b arrl' a' b' sub, Substitute arrr c d arrr' c' d' sub) =>
    Substitute (ArrowSSS' arrl arrr) (PN a c) (PN b d) (ArrowSSS' arrl' arrr') (PN a' c') (PN b' d') sub where
    substitute (f :***:: g) sub = substitute f sub :***:: substitute g sub

instance (Substitute arr (PN a c) (PN b c) arr' (PN a' c') (PN b' c') sub) =>
    Substitute (ArrowLoop' arr c) a b (ArrowLoop' arr' c') a' b' sub where
    substitute (Loop' f) sub = Loop' (substitute f sub)

-- MAKEMIO - from AFRP' to MIO computations

type ReadCells :: forall (ar :: Arity). Desc' ar -> MemState -> Constraint
class ReadCells ac res | ac -> res where
    readCells :: Proxy ac -> MIO res (Val (AsDesc ac))

instance ReadCells (VN n a) '( '[], '[IOCell n a], '[]) where
    readCells _ = One <$> readIOCell

instance (ReadCells lc lr, ReadCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => ReadCells (PN lc rc) res where
    readCells prox = Rearrange.do
        let (lp, rp) = splitProx prox
        left <- readCells lp
        Pair left <$> readCells rp

type WriteCells :: forall (ar :: Arity). Desc' ar -> MemState -> Constraint
class WriteCells ac res | ac -> res where
    writeCells :: Proxy ac -> Val (AsDesc ac) -> MIO res ()

instance WriteCells (VN n a) '( '[IOCell n a], '[], '[]) where
    writeCells _ (One v) = writeIOCell v

instance (WriteCells lc lr, WriteCells rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => WriteCells (PN lc rc) res where
    writeCells prox (Pair a b) = Rearrange.do
        let (l, r) = splitProx prox
        writeCells l a
        writeCells r b

type WriteCellsAfter :: forall (ar :: Arity). Desc' ar -> MemState -> Constraint
class WriteCellsAfter ac res | ac -> res where
    writeCellsAfter :: Proxy ac -> Val (AsDesc ac) -> MIO res ()

instance WriteCellsAfter (VN n a) '( '[], '[], '[IOCell n a]) where
    writeCellsAfter _ (One v) = writeIOCellAfter v

instance (WriteCellsAfter lc lr, WriteCellsAfter rc rr, res ~ MemoryPlus lr rr, MemoryInv lr rr)
    => WriteCellsAfter (PN lc rc) res where
    writeCellsAfter prox (Pair a b) = Rearrange.do
        let (l, r) = splitProx prox
        writeCellsAfter l a
        writeCellsAfter r b

class ProxToRef ac env where
    proxToRef :: Proxy ac -> HList env -> Ref ac

instance (LookupNth n env (IOCell n a)) => ProxToRef (VN n a) env where
    proxToRef Proxy env =
        let cell = lookupNth (Proxy :: Proxy n) env in VRef cell

instance (ProxToRef l env, ProxToRef r env) => ProxToRef (PN l r) env where
    proxToRef prox env = let (pl, pr) = splitProx prox in
        PRef (proxToRef pl env) (proxToRef pr env)

-- Since the type of the Memory computation as output is going to differ based on which
-- Arrow combinator we use, we need a different case for each combinator.

-- NOTE: We cannot do this by having lowercase arr etc. instead of transforming a special GADT.
-- This is because the types get very difficult for a user to specify: 
-- If we just have e.g. lowercase arr :: (Val a -> Val b) -> Ref ans a -> IO (HList prog, Ref bns b)
-- then a user will have to type `arr f bs rf` with the correct fs' and prog.
-- There might be a world where a user can say that the type is (Arr <a bunch of arguments>) but that seems worse than the current one.

type AsMemory :: forall (ar :: Arity) (br :: Arity).
    Arrow' ar br -> Desc' ar -> Desc' br -> [*] -> [*] -> Constraint
class AsMemory arr a b env prog | arr a env -> b prog where
    toProgram :: AFRP' arr a b -> Proxy a -> HList env -> IO (HList prog, Proxy b)

instance AsMemory ArrowId' a a env '[] where
    toProgram Id' prox _ = Prelude.return (HNil, prox)

instance AsMemory ArrowDropL' (PN a b) b env '[] where
    toProgram DropL' prox _ = let (_, br) = splitProx prox in Prelude.return (HNil, br)

instance AsMemory ArrowDropR' (PN a b) a env '[] where
    toProgram DropR' prox _ = let (ar, _) = splitProx prox in Prelude.return (HNil, ar)

instance AsMemory ArrowDup' a (PN a a) env '[] where
    toProgram Dup' prox _ = Prelude.return (HNil, pairProx prox prox)

instance (ProxToRef a env) => AsMemory (ArrowConst' a) x a env '[] where
    toProgram (Constant' c) _ env = do
        let outprox = Proxy :: Proxy a
        flip writeRef c $ proxToRef outprox env
        Prelude.return (HNil, outprox)

instance (ReadCells a ar, WriteCells b br,
    MemoryInv ar br, prog ~ '[MIO (MemoryPlus ar br) ()]) =>
    AsMemory (ArrowArr' b) a b env prog where
        toProgram (Arr' f) inprox _ = do
            let outprox = Proxy :: Proxy b
                comp :: MIO (MemoryPlus ar br) ()
                comp = (f <$> readCells inprox) Rearrange.>>= writeCells outprox
            Prelude.return (comp :+: HNil, outprox)

instance (ReadCells a ar, WriteCellsAfter a' ar', AsDesc a' ~ AsDesc a,
    MemoryInv ar ar', mems ~ MemoryPlus ar ar', ProxToRef a' env) =>
    AsMemory (ArrowPre' a') a a' env '[MIO mems ()] where
        toProgram (Pre' v) inprox env = do
            let outprox = Proxy :: Proxy a'
                comp = readCells inprox Rearrange.>>= writeCellsAfter outprox
            flip writeRef v $ proxToRef (Proxy :: Proxy a') env
            Prelude.return (comp :+: HNil, outprox)

instance (AsMemory larr a b env progl, AsMemory rarr b c env progr,
    prog ~ Combine progl progr) =>
    AsMemory (ArrowGGG' larr rarr) a c env prog where
        toProgram (f :>>>:: g) inprox env = do
            (compf, midprox) <- toProgram f inprox env
            (compg, outprox) <- toProgram g midprox env
            Prelude.return (hCombine compf compg, outprox)

instance (AsMemory larr la lb env progl, AsMemory rarr ra rb env progr,
    prog ~ Combine progl progr) =>
    AsMemory (ArrowSSS' larr rarr) (PN la ra) (PN lb rb) env prog where
        toProgram (f :***:: g) prox env = do
            let (inl, inr) = splitProx prox
            (compf, outl) <- toProgram f inl env
            (compg, outr) <- toProgram g inr env
            Prelude.return (hCombine compf compg, pairProx outl outr)

instance (AsMemory arr (PN a c) (PN b c) env prog) =>
    AsMemory (ArrowLoop' arr c) a b env prog where
        toProgram (Loop' f) inref env = do
            (comp, prox') <- toProgram f (pairProx inref (Proxy :: Proxy c)) env
            let (out, _) = splitProx prox'
            Prelude.return (comp, out)

-- The output cell of a pre will always contain the _next_ value, since it is written at the end of each run
-- through writeCellsAfter (this is needed to break the loop - we think of this as the pre preloading the next
-- value to return).
-- This means that if have the output cell of a pre as the output of the entire program, reading it will give
-- us the _next_ value being returned, not the current one.
-- We fix this by adding one separate output cell by postcomposing >>> arr id to the input.
type Augment :: [*] -> Desc' a -> FreshState -> [*] -> Desc' a -> FreshState -> Constraint
class Augment prog a bs prog' a' bs' | prog a bs -> prog' a' bs' where
    augment :: HList prog -> Proxy a -> BuildState bs -> IO (HList prog', Proxy a', BuildState bs')

instance forall a a' fs fs' read write prog prog'.
    (Fresh (AsDesc a) fs a' fs',
    ReadCells a read, WriteCells a' write,
    MemoryInv read write,
    AsDesc a ~ AsDesc a',
    prog' ~ Append (MIO (MemoryPlus read write) ()) prog) =>
    Augment prog a fs prog' a' fs' where
        augment prog prox bs = do
            (prox', bs') <- fresh bs (Proxy :: Proxy (AsDesc a))
            let comp = readCells prox Rearrange.>>= writeCells prox'
            Prelude.return (hAppend comp prog, prox', bs')