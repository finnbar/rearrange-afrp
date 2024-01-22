{-# LANGUAGE BangPatterns #-}

-- A simple benchmark program which runs Test0's programs on 100_000 inputs.
-- Chupin runs their whole suite six times over six different switching values and six different sizes.
-- We can automate this process similarly, I just hope it's happy appending to a CSV file in doing so.

import Test0

import Criterion
import Criterion.Main
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import RAFRP
import GraphRearrange (buildGraphAndTopsort)
import FRP.Yampa
import Control.DeepSeq
import Control.Monad (replicateM_)
import Data.IORef

-- NOTE: May want to move to doubles to force more computation.

main :: IO ()
main = do
    !inps <- Gen.sample $ Gen.list (Range.singleton 100000) $ Gen.integral (Range.linear 0 10000)

    -- Build the AFRP runner
    (prog, env, inref, outref) <- toRearrangeable afrp
    let !runAFRP = buildGraphAndTopsort prog env
        af inp = do
            writeRef inref inp
            runAFRP
            readRef outref
        !inps' = map One inps
    
    defaultMain $ benches yampa af inps inps'

benches :: SF Int Int -> (Val (V Int) -> IO (Val (V Int))) -> [Int] -> [Val (V Int)] -> [Benchmark]
benches sf afrp inps inps' = [
        bench "sf" $ nfIO (benchSF sf inps),
        bench "afrp" $ nfIO (benchAFRP afrp inps')
    ]

-- Each benchX works by allocating our 100000 values to an IORef, and then popping from it at each step.
-- This means that every benchmark performs a read from memory - if only the compiled AFRP did this it would disadvantage it.
-- This is the same technique as in Chupin and Nilsson's SFRP.
-- (see in particular: https://gitlab.com/chupin/scalable-frp/-/blob/master/src/EnclRunTest.hs)
benchAFRP :: (Val (V Int) -> IO (Val (V Int))) -> [Val (V Int)] -> IO ()
benchAFRP !runner ins = do 
    inputRef <- newIORef ins
    replicateM_ (length ins) $ do
        (i : inps) <- readIORef inputRef
        writeIORef inputRef inps
        One out <- runner i
        forceM out

benchSF :: SF Int Int -> [Int] -> IO ()
benchSF sf ins = do
    inputRef <- newIORef ins
    handle <- reactInit (return 0) (\handle' _ v -> forceM v >> return True) sf
    replicateM_ (length ins) $ do
        (i : inps) <- readIORef inputRef
        writeIORef inputRef inps
        b <- react handle (1, Just i)
        forceM b

forceM :: (Monad m, NFData a) => a -> m ()
forceM val = case rnf val of
    () -> pure ()