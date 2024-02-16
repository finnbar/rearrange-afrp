{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  rec
    _4 <- iPre 2.685924937052612 -< _3
    _3 <- iPre 5.355242900069075 -< _4
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _4)
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _0)
  _7 <- FRP.Yampa.arr (+1) -< _4
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _5)
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _8)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  rec
    _4 <- pre1 2.685924937052612 -< _3
    _3 <- pre1 5.355242900069075 -< _4

  _5 <- arr21 (-) -< {_3, _4}
  _6 <- arr21 (-) -< {_4, _0}
  _7 <- arr11 (+1) -< _4
  _8 <- arr21 (*) -< {_6, _5}
  _9 <- arr21 (-) -< {_7, _8}
  _10 <- arr21 (*) -< {_2, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2