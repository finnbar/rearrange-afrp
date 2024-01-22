{-# LANGUAGE BangPatterns #-}

-- A simple benchmark program which runs Test0's programs on 100_000 inputs.
-- Chupin runs their whole suite six times over six different switching values and six different sizes.
-- We can automate this process similarly, I just hope it's happy appending to a CSV file in doing so.

import Test0

import Criterion.Main
import Criterion.Types (Config(..), Verbosity(..))
import Statistics.Types (cl99)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import RAFRP
import GraphRearrange (buildGraphAndTopsort)
import FRP.Yampa
import Control.DeepSeq
import Control.Monad (replicateM_)
import Data.IORef

main :: IO ()
main = do
    !inps <- Gen.sample $ Gen.list (Range.singleton 100000) $ Gen.double (Range.exponentialFloat 0 100)

    -- Build the AFRP runner
    (prog, env, inref, outref) <- toRearrangeable afrp
    let !runAFRP = buildGraphAndTopsort prog env
        af inp = do
            writeRef inref inp
            runAFRP
            readRef outref
        !inps' = map One inps
    
    defaultMainWith (defaultConfig {csvFile = Just "tests.csv", confInterval = cl99}) $ benches yampa af inps inps' (codeLen, codeRecLen)

benches :: SF Double Double -> (Val (V Double) -> IO (Val (V Double))) -> [Double] -> [Val (V Double)] -> (Int, Int) -> [Benchmark]
benches sf afrp inps inps' params = [
        bench ("sf" ++ show params) $ nfIO (benchSF sf inps),
        bench ("afrp" ++ show params) $ nfIO (benchAFRP afrp inps')
    ]

-- Each benchX works by allocating our 100000 values to an IORef, and then popping from it at each step.
-- This is the same technique as in Chupin and Nilsson's SFRP.
-- (see in particular: https://gitlab.com/chupin/scalable-frp/-/blob/master/src/EnclRunTest.hs)
benchAFRP :: (Val (V Double) -> IO (Val (V Double))) -> [Val (V Double)] -> IO ()
benchAFRP !runner ins = do 
    inputRef <- newIORef ins
    replicateM_ (length ins) $ do
        (i : inps) <- readIORef inputRef
        writeIORef inputRef inps
        One out <- runner i
        forceM out

benchSF :: SF Double Double -> [Double] -> IO ()
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