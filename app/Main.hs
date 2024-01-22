module Main (main) where

import System.Environment (getArgs)
import ToAFRPCode (generateFile)

main :: IO ()
main = do
    -- Get the first two args
    l : rl : _ <- getArgs
    let len :: Int
        len = read l
        recLenFrac :: Double
        recLenFrac = read rl
        recLen :: Int
        recLen = round (fromIntegral len * recLenFrac)
    generateFile len recLen "Test0"