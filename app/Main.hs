{-# LANGUAGE DataKinds, PartialTypeSignatures #-}

module Main (main) where

import RAFRP

arr1 :: AFRP _ (V Int) (P (V Int) (V Int))
arr1 = Arr (\(One x) -> let y = x + 1 in Pair (One y) (One y))

prog1 :: AFRP _ (V Int) (P (V Int) (V Int))
prog1 = arr1 :>>>: (Arr (\(One x) -> One (x + 1)) :***: Pre (One 1))

prog1Run :: IO (Val (V Int) -> IO (Val (P (V Int) (V Int))))
prog1Run = makeAFRP prog1 >>= makeRunnable

arr2 :: AFRP _ (P (V Int) (V Int)) (P (V Int) (V Int))
arr2 = Arr (\(Pair (One x) (One y)) -> let z = x + y in Pair (One z) (One z))

prog2 :: AFRP _ (V Int) (V Int)
prog2 = Loop $ arr2 :>>>: (Id :***: Pre (One 0))

prog2Run :: IO (Val (V Int) -> IO (Val (V Int)))
prog2Run = makeAFRP prog2 >>= makeRunnable

arr3 :: AFRP _ (P (V Int) (V Int)) (V Int)
arr3 = Arr (\(Pair (One x) (One y)) -> One $ x + y)

prog3 :: AFRP _ (V Int) (V Int)
prog3 = arr1 :>>>: Dup :>>>: (arr2 :***: Pre (Pair (One 1) (One 2))) :>>>: (DropL :***: DropL) :>>>: arr3

prog3Run :: IO (Val (V Int) -> IO (Val (V Int)))
prog3Run = makeAFRP prog3 >>= makeRunnable

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

-- [5,1], [6,4], 3, 7, 9, 14