{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _1)
    _5 <- FRP.Yampa.arr (+1) -< _4
    _6 <- FRP.Yampa.arr (+1) -< _2
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _5)
    _8 <- iPre 1.9303755846559703 -< _7
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _1)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _1)
    _11 <- iPre 0.11620149773999996 -< _0
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _9)
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _5)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _8)
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _12)
    _16 <- FRP.Yampa.arr (+1) -< _7
    _2 <- iPre 2.081238509247857 -< _0
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _13)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _17)
  _19 <- FRP.Yampa.arr (+1) -< _11
  _20 <- FRP.Yampa.arr (+1) -< _15
  _21 <- FRP.Yampa.arr (+1) -< _19
  _22 <- FRP.Yampa.arr (+1) -< _20
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _16)
  _24 <- FRP.Yampa.arr (+1) -< _18
  _25 <- iPre 1.2648148470624871 -< _24
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _25)
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _14)
  _28 <- iPre 1.460973729448489 -< _27
  _29 <- FRP.Yampa.arr (+1) -< _22
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _28)
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  rec
    _3 <- arr21 (-) -< {_1, _2}
    _4 <- arr21 (-) -< {_3, _1}
    _5 <- arr11 (+1) -< _4
    _6 <- arr11 (+1) -< _2
    _7 <- arr21 (*) -< {_6, _5}
    _8 <- pre1 1.9303755846559703 -< _7
    _9 <- arr21 (+) -< {_8, _1}
    _10 <- arr21 (*) -< {_9, _1}
    _11 <- pre1 0.11620149773999996 -< _0
    _12 <- arr21 (-) -< {_4, _9}
    _13 <- arr21 (*) -< {_0, _5}
    _14 <- arr21 (*) -< {_5, _8}
    _15 <- arr21 (-) -< {_12, _12}
    _16 <- arr11 (+1) -< _7
    _2 <- pre1 2.081238509247857 -< _0

  _17 <- arr21 (+) -< {_10, _13}
  _18 <- arr21 (+) -< {_2, _17}
  _19 <- arr11 (+1) -< _11
  _20 <- arr11 (+1) -< _15
  _21 <- arr11 (+1) -< _19
  _22 <- arr11 (+1) -< _20
  _23 <- arr21 (+) -< {_21, _16}
  _24 <- arr11 (+1) -< _18
  _25 <- pre1 1.2648148470624871 -< _24
  _26 <- arr21 (+) -< {_23, _25}
  _27 <- arr21 (*) -< {_26, _14}
  _28 <- pre1 1.460973729448489 -< _27
  _29 <- arr11 (+1) -< _22
  _30 <- arr21 (*) -< {_29, _28}
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15