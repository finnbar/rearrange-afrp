{-# LANGUAGE DataKinds, PartialTypeSignatures #-}

module Main (main) where

import RAFRP
import Rearrange

-- TODO idk how to fix this.
-- The issue is to do with typing `arr1` - we don't know the types of any of the indices so cannot reduce any of the type families.
-- This means that we're unable to solve the constraint (s ~ Nub (Sort s)) since we cannot reduce s to see that it is obvious.
-- Maybe we can try to add some AsSet constraints throughout?
-- OR could we provide a type that sets the relevant indices somehow?
-- It would also be nice to avoid having _so many_ underscores.
-- Might be able to write types like e.g.
-- Arr (V Int) (P (V Int) (V Int)) = RAFRP (V' a Int) (P' (V' b Int) (V' c Int))) [<read a, write b and c>] [<env containing a, b and c>] n (n+varcount)
-- and so on so that the types match the value-level structure.
-- Try to separate descriptors from names again, even if that means some kind of descriptor proxy.
-- This simplifies the types a bit and hopefully means things will behave better.
-- That's significant work though.

arr1 :: RAFRP (V _ Int) (P (V _ Int) (V _ Int)) _ _ _ _
arr1 = arr (\(One x) -> let y = x + 1 in Pair (One y) (One y))

prog1 :: RAFRP (V _ Int) (P (V _ Int) (V _ Int)) _ _ _ _
prog1 = arr1 >>> (arr (\(One x) -> One x + 1) *** pre (One 1))

main :: IO ()
main = do
    CRAFRP env prog inref outref <- makeRAFRP prog1
    program <- makeProgram prog env
    writeRef inref (One 3)
    runProgram_ program
    print <$> readRef outref
