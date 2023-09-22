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