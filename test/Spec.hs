{-# LANGUAGE BangPatterns #-}

import Test0
import RAFRP
import FRP.Yampa

-- TODO Turn this into an actual test suite.

afrpVsYampa :: SF Int Int -> (Val (V Int) -> IO (Val (V Int))) -> [Int] -> [Val (V Int)] -> IO ()
afrpVsYampa ymp af ins ins' = do
    let ympRes = embed ymp (deltaEncode 1 ins)
    afrpRes <- mapM af ins'
    print ympRes
    print afrpRes
    print (ympRes == map (\(One x) -> x) afrpRes)

main :: IO ()
main = do
    -- !runAFRP <- makeAFRP afrp
    -- let inps = [1..50]
    --     inps' = map One inps
    -- afrpVsYampa yampa runAFRP inps inps'
    -- (prog, env, inref, outref) <- toRearrangeable afrp
    putStrLn "Commented out for performance reasons."
