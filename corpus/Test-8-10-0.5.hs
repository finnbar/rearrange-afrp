{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  rec
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
    _6 <- iPre 1.8049870613571952 -< _0
    _2 <- iPre 1.8062919087821643 -< _4
    _3 <- iPre 0.8657212992795517 -< _2
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _5)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _3)
  _9 <- FRP.Yampa.arr (+1) -< _6
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  rec
    _4 <- arr21 (*) -< {_0, _0}
    _5 <- arr21 (-) -< {_0, _2}
    _6 <- pre1 1.8049870613571952 -< _0
    _2 <- pre1 1.8062919087821643 -< _4
    _3 <- pre1 0.8657212992795517 -< _2

  _7 <- arr21 (*) -< {_1, _5}
  _8 <- arr21 (*) -< {_7, _3}
  _9 <- arr11 (+1) -< _6
  _10 <- arr21 (+) -< {_8, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5