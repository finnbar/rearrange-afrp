{-# LANGUAGE DataKinds, PartialTypeSignatures #-}

module Main (main) where

import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH (gap)

arr1 :: AFRP _ (V Int) (P (V Int) (V Int))
arr1 = Arr (\(One x) -> let y = x + 1 in Pair (One y) (One y))

prog1 :: AFRP _ (V Int) (P (V Int) (V Int))
prog1 = arr1 :>>>: (Arr (\(One x) -> One (x + 1)) :***: Pre (One 1))

prog1Run :: IO (Val (V Int) -> IO (Val (P (V Int) (V Int))))
prog1Run = makeAFRP prog1

arr2 :: AFRP _ (P (V Int) (V Int)) (P (V Int) (V Int))
arr2 = Arr (\(Pair (One x) (One y)) -> let z = x + y in Pair (One z) (One z))

prog2 :: AFRP _ (V Int) (V Int)
prog2 = Loop (arr2 :>>>: (Id :***: Pre (One 0)))

prog2Run :: IO (Val (V Int) -> IO (Val (V Int)))
prog2Run = makeAFRP prog2

arr3 :: AFRP _ (P (V Int) (V Int)) (V Int)
arr3 = Arr (\(Pair (One x) (One y)) -> One $ x + y)

prog3 :: AFRP _ (V Int) (V Int)
prog3 = arr1 :>>>: Dup :>>>: (arr2 :***: Pre (Pair (One 1) (One 2))) :>>>: (DropL :***: DropL) :>>>: arr3

prog3Run :: IO (Val (V Int) -> IO (Val (V Int)))
prog3Run = makeAFRP prog3

prog4 :: AFRP _ (P (V Int) (V Int)) (V Int)
prog4 = [gap|proc {x,y} -> do
        {z, w} <- arr1 -< x
        arr3 -< {z, y}
    |]

prog4Run :: IO (Val (P (V Int) (V Int)) -> IO (Val (V Int)))
prog4Run = makeAFRP prog4

prog5 :: AFRP _ (V Int) (V Int)
prog5 = [gap|proc x -> do
        rec
            z <- arr3 -< {x, prev}
            prev <- pre (One 0) -< z
        returnA -< z
    |]

prog5Run :: IO (Val (V Int) -> IO (Val (V Int)))
prog5Run = makeAFRP prog5

main :: IO ()
main = do
    runnable1 <- prog1Run
    out11 <- runnable1 (One 3)
    out21 <- runnable1 (One 4)
    print out11
    print out21

    runnable2 <- prog2Run
    out12 <- runnable2 (One 3)
    out22 <- runnable2 (One 4)
    print out12
    print out22

    runnable3 <- prog3Run
    out13 <- runnable3 (One 3)
    out23 <- runnable3 (One 4)
    print out13
    print out23

    runnable4 <- prog4Run
    out14 <- runnable4 (Pair (One 3) (One 4))
    out24 <- runnable4 (Pair (One 6) (One 5))
    print out14
    print out24

    runnable5 <- prog5Run
    out15 <- runnable5 (One 3)
    out25 <- runnable5 (One 4)
    print out15
    print out25

-- [5,1], [6,4], 3, 7, 10, 14, 8, 12, 3, 7