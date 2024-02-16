{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  rec
    _3 <- FRP.Yampa.arr (+1) -< _1
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
    _6 <- FRP.Yampa.arr (+1) -< _3
    _2 <- iPre 1.5693741188152643 -< _2
  _7 <- FRP.Yampa.arr (+1) -< _2
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _2)
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _5)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _8)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  rec
    _3 <- arr11 (+1) -< _1
    _4 <- arr21 (+) -< {_0, _2}
    _5 <- arr21 (+) -< {_4, _1}
    _6 <- arr11 (+1) -< _3
    _2 <- pre1 1.5693741188152643 -< _2

  _7 <- arr11 (+1) -< _2
  _8 <- arr21 (*) -< {_7, _2}
  _9 <- arr21 (-) -< {_6, _5}
  _10 <- arr21 (-) -< {_9, _8}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5