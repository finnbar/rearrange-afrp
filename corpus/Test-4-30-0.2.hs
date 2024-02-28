{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _5 <- iPre 2.116704564279905 -< _4
  rec
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _4)
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _6)
    _9 <- iPre 1.51492719158018 -< _6
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _5)
    _6 <- iPre 1.4329611272339957 -< _0
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _1)
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _2)
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _5)
  _15 <- FRP.Yampa.arr (+1) -< _0
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _14)
  _17 <- FRP.Yampa.arr (+1) -< _13
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _10)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _3)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _17)
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _18)
  _22 <- iPre 2.9426240014413048 -< _15
  _23 <- FRP.Yampa.arr (+1) -< _9
  _24 <- FRP.Yampa.arr (+1) -< _19
  _25 <- iPre 2.8006762219575583 -< _22
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _25)
  _27 <- iPre 2.296527798384665 -< _26
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _27)
  _29 <- iPre 1.8768134834675876 -< _28
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _24)
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (+) -< {_0, _1}
  _3 <- arr21 (*) -< {_1, _1}
  _4 <- arr21 (*) -< {_0, _0}
  _5 <- pre1 2.116704564279905 -< _4
  rec
    _7 <- arr21 (+) -< {_0, _4}
    _8 <- arr21 (-) -< {_6, _6}
    _9 <- pre1 1.51492719158018 -< _6
    _10 <- arr21 (+) -< {_0, _0}
    _11 <- arr21 (-) -< {_8, _5}
    _6 <- pre1 1.4329611272339957 -< _0

  _12 <- arr21 (-) -< {_11, _1}
  _13 <- arr21 (-) -< {_7, _2}
  _14 <- arr21 (-) -< {_12, _5}
  _15 <- arr11 (+1) -< _0
  _16 <- arr21 (-) -< {_0, _14}
  _17 <- arr11 (+1) -< _13
  _18 <- arr21 (*) -< {_10, _10}
  _19 <- arr21 (-) -< {_16, _3}
  _20 <- arr21 (*) -< {_13, _17}
  _21 <- arr21 (-) -< {_20, _18}
  _22 <- pre1 2.9426240014413048 -< _15
  _23 <- arr11 (+1) -< _9
  _24 <- arr11 (+1) -< _19
  _25 <- pre1 2.8006762219575583 -< _22
  _26 <- arr21 (-) -< {_21, _25}
  _27 <- pre1 2.296527798384665 -< _26
  _28 <- arr21 (*) -< {_23, _27}
  _29 <- pre1 1.8768134834675876 -< _28
  _30 <- arr21 (-) -< {_29, _24}
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6