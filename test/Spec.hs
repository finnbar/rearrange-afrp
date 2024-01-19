{-# LANGUAGE BangPatterns #-}

import Test0
import RAFRP
import FRP.Yampa
import GraphRearrange (buildGraphAndTopsort)

-- TODO Turn this into an actual test suite.
-- NOTE: It might be worth investigating the performance of Naming and MakeMIO at some point, but Rearrange needs fixing first.
-- NOTE: It seems like getting Set involved REALLY breaks performance. Interesting.
-- My expectation is that it's because a lot of invariants are enforced on Set, which require a lot of memory to enumerate.

-- TODO: Removing the use of Set solves all of our issues. Can we modify the original Rearrange to not use Set too?
-- Will that fix our issues?

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
    let inps = [1..50]
        inps' = map One inps
    -- afrpVsYampa yampa runAFRP inps inps'
    (prog, env, inref, outref) <- toRearrangeable afrp
    let runAFRP = buildGraphAndTopsort prog env
        af inp = do
            writeRef inref inp
            runAFRP
            readRef outref
    afrpVsYampa yampa af inps inps'
