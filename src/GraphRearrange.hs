{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, Strict #-}

-- A hastily-added value-level version of rearrange.
-- This replaces the type-level rearrange, which actively does not compile on larger examples
-- as GHC runs out of memory. 
module GraphRearrange where

import Data.Graph
import Data.List (intersect)
import Data.Maybe (mapMaybe)
import Data.Memory.Types
import Data.Memory.RunMemory
import Data.Type.HList
import Data.Type.Set (Set(..))
import Data.Proxy
import GHC.TypeLits (KnownNat, natVal)
import Debug.Trace

class ReifyCellList xs where
    reifyCellList :: Proxy xs -> [Int]

instance ReifyCellList '[] where
    reifyCellList Proxy = []

instance (n ~ GetName x, KnownNat n, ReifyCellList xs) => ReifyCellList (x ': xs) where
    reifyCellList Proxy = fromIntegral (natVal (Proxy :: Proxy n)) : reifyCellList (Proxy :: Proxy xs)

-- Brings our type information to the value level.
class MemsToIODependencies xs env where
    memsToIODependencies :: HList xs -> HList env -> [(IO (), [Int], [Int], [Int])]

instance MemsToIODependencies '[] env where
    memsToIODependencies HNil _ = []

instance (SortedHListToSubset env (MemoryUnion '(writes, reads, postwrites)), MemsToIODependencies xs env,
    ReifyCellList writes, ReifyCellList reads, ReifyCellList postwrites)
    => MemsToIODependencies (Memory IO '(writes, reads, postwrites) () ': xs) env where
    memsToIODependencies (mem :+: mems) env =
        (runMemory mem (sortedHListToSubset env),
        reifyCellList (Proxy :: Proxy writes),
        reifyCellList (Proxy :: Proxy reads),
        reifyCellList (Proxy :: Proxy postwrites))
            : memsToIODependencies mems env

assignKeys :: [(IO (), [Int], [Int], [Int])] -> [(IO (), Int, ([Int], [Int], [Int]))]
assignKeys = zipWith joiner [1..]
    where
        joiner k (io, wr, rd, pwr) = (io, k, (wr, rd, pwr))

-- Resolve dependencies by having edges where:
-- a computation that writes w -> a computation that reads w
-- a computation that reads w -> a computation that postwrites w
resolveDependencies :: [(IO (), Int, ([Int], [Int], [Int]))] -> [(IO (), Int, [Int])]
resolveDependencies mems = map findDeps mems
    where
        findDeps :: (IO (), Int, ([Int], [Int], [Int])) -> (IO (), Int, [Int])
        findDeps (io, k, sets) = (io, k, mapMaybe (isDependent sets) mems)

        isDependent :: ([Int], [Int], [Int]) -> (IO (), Int, ([Int], [Int], [Int])) -> Maybe Int
        isDependent (wr, rd, _) (_, k, (_, rd', pwr'))
            | null (intersect wr rd') && null (intersect rd pwr') = Nothing
            | otherwise = Just k

buildGraphAndTopsort :: MemsToIODependencies xs env => HList xs -> HList env -> IO ()
buildGraphAndTopsort mems env = let
    reified = memsToIODependencies mems env
    withKeys = assignKeys reified
    (graph, fromVertex, _) = graphFromEdges (resolveDependencies withKeys)
    sortedVertices = trace "topsort" $ topSort graph
    computations = map (\vert -> let (io, _, _) = fromVertex vert in io) sortedVertices
    in trace "built graph" $ sequence_ computations

-- NOTE: This enforces NONE of the Set requirements.
-- We use this since we know the HList to be sorted and without repeats, and calling the Set constraints
-- seems to be part of the reason that GHC is panicking.
class SortedHListToSubset xs set where
    sortedHListToSubset :: HList xs -> Set set

instance SortedHListToSubset '[] '[] where
    sortedHListToSubset _ = Empty

instance {-# OVERLAPS #-} SortedHListToSubset xs ss => SortedHListToSubset (x ': xs) (x ': ss) where
    sortedHListToSubset (x :+: xs) = Ext x $ sortedHListToSubset xs

instance {-# OVERLAPPING #-} SortedHListToSubset xs ss => SortedHListToSubset (x ': xs) ss where
    sortedHListToSubset (_ :+: xs) = sortedHListToSubset xs