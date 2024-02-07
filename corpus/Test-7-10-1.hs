{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _5 <- FRP.Yampa.arr (+1) -< _0
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _4)
    _7 <- iPre 1.637352301938182 -< _5
    _8 <- iPre 6.843649628246269e-3 -< _6
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
    _10 <- iPre 2.087956249241787 -< _5
    _1 <- iPre 2.948820859544375 -< _8
    _2 <- iPre 0.9766187419011524 -< _7
    _3 <- iPre 0.8272659054978305 -< _10
    _4 <- iPre 1.2073039662740008 -< _3
  FRP.Yampa.returnA -< _9

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _5 <- arr11 (+1) -< _0
    _6 <- arr21 (-) -< {_2, _4}
    _7 <- pre1 1.637352301938182 -< _5
    _8 <- pre1 6.843649628246269e-3 -< _6
    _9 <- arr21 (-) -< {_0, _1}
    _10 <- pre1 2.087956249241787 -< _5
    _1 <- pre1 2.948820859544375 -< _8
    _2 <- pre1 0.9766187419011524 -< _7
    _3 <- pre1 0.8272659054978305 -< _10
    _4 <- pre1 1.2073039662740008 -< _3

  GenProc.GeneralisedArrow.returnA -< _9|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10