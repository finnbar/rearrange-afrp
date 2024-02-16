{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 21.19838502202334 -< _0
  rec
    _3 <- iPre 1.0771029132153642 -< _2
    _2 <- iPre 32.046851296915875 -< _3
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _1)
  _6 <- iPre 39.14651374868676 -< _4
  _7 <- iPre 12.441703604303429 -< _5
  _8 <- FRP.Yampa.arr (+1) -< _6
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _7)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 21.19838502202334 -< _0
  rec
    _3 <- pre1 1.0771029132153642 -< _2
    _2 <- pre1 32.046851296915875 -< _3

  _4 <- arr21 (+) -< {_1, _1}
  _5 <- arr21 (+) -< {_2, _1}
  _6 <- pre1 39.14651374868676 -< _4
  _7 <- pre1 12.441703604303429 -< _5
  _8 <- arr11 (+1) -< _6
  _9 <- arr11 (+1) -< _8
  _10 <- arr21 (+) -< {_9, _7}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2