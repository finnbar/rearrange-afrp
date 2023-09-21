{-# LANGUAGE DataKinds, PartialTypeSignatures #-}

module Main (main) where

import RAFRP
import Rearrange

arr1 :: AFRP _ (V Int) (P (V Int) (V Int))
arr1 = Arr (\(One x) -> let y = x + 1 in Pair (One y) (One y))

prog1 :: AFRP _ (V Int) (P (V Int) (V Int))
prog1 = arr1 :>>>: (Arr (\(One x) -> One (x + 1)) :***: Pre (One 1))

prog1Run :: IO (Val (V Int) -> IO (Val (P (V Int) (V Int))))
prog1Run = makeAFRP prog1 Prelude.>>= makeRunnable

arr2 :: AFRP _ (P (V Int) (V Int)) (P (V Int) (V Int))
arr2 = Arr (\(Pair (One x) (One y)) -> let z = x + y in Pair (One z) (One z))

prog2 :: AFRP _ (V Int) (V Int)
prog2 = Loop $ arr2 :>>>: (Id :***: Pre (One 0))

prog2Run :: IO (Val (V Int) -> IO (Val (V Int)))
prog2Run = makeAFRP prog2 Prelude.>>= makeRunnable

main :: IO ()
main = do
    runnable <- prog1Run
    out1 <- runnable (One 3)
    out2 <- runnable (One 4)
    print out1
    print out2

    runnable <- prog2Run
    out1 <- runnable (One 3)
    out2 <- runnable (One 4)
    print out1
    print out2