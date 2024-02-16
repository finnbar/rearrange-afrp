{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  rec
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _6 <- iPre 1.7755684224773738 -< _5
    _7 <- FRP.Yampa.arr (+1) -< _0
    _8 <- FRP.Yampa.arr (+1) -< _3
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
    _10 <- iPre 9.857131661716391e-2 -< _2
    _11 <- FRP.Yampa.arr (+1) -< _10
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _5)
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _6)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _1)
    _15 <- FRP.Yampa.arr (+1) -< _12
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _4)
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _0)
    _18 <- FRP.Yampa.arr (+1) -< _9
    _4 <- iPre 2.93585483616452 -< _18
  _19 <- iPre 1.8446935922543435 -< _16
  _20 <- FRP.Yampa.arr (+1) -< _1
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _19)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _21)
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _22)
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _23)
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _8)
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _25)
  _27 <- FRP.Yampa.arr (+1) -< _26
  _28 <- iPre 0.44240310076862116 -< _27
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 2.4796913399544946 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- arr21 (+) -< {_0, _0}
  rec
    _5 <- arr21 (+) -< {_0, _0}
    _6 <- pre1 1.7755684224773738 -< _5
    _7 <- arr11 (+1) -< _0
    _8 <- arr11 (+1) -< _3
    _9 <- arr21 (*) -< {_2, _1}
    _10 <- pre1 9.857131661716391e-2 -< _2
    _11 <- arr11 (+1) -< _10
    _12 <- arr21 (*) -< {_7, _5}
    _13 <- arr21 (*) -< {_11, _6}
    _14 <- arr21 (+) -< {_6, _1}
    _15 <- arr11 (+1) -< _12
    _16 <- arr21 (-) -< {_3, _4}
    _17 <- arr21 (+) -< {_12, _0}
    _18 <- arr11 (+1) -< _9
    _4 <- pre1 2.93585483616452 -< _18

  _19 <- pre1 1.8446935922543435 -< _16
  _20 <- arr11 (+1) -< _1
  _21 <- arr21 (+) -< {_13, _19}
  _22 <- arr21 (-) -< {_14, _21}
  _23 <- arr21 (-) -< {_17, _22}
  _24 <- arr21 (+) -< {_15, _23}
  _25 <- arr21 (*) -< {_24, _8}
  _26 <- arr21 (-) -< {_20, _25}
  _27 <- arr11 (+1) -< _26
  _28 <- pre1 0.44240310076862116 -< _27
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 2.4796913399544946 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15