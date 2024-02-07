{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (+1) -< _0
  _4 <- iPre 2.8055047138551292 -< _1
  rec
    _9 <- FRP.Yampa.arr (+1) -< _0
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _2)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _8)
    _12 <- FRP.Yampa.arr (+1) -< _10
    _13 <- FRP.Yampa.arr (+1) -< _4
    _14 <- FRP.Yampa.arr (+1) -< _10
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _12)
    _16 <- iPre 2.1911412840156745 -< _14
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _11)
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _13)
    _19 <- FRP.Yampa.arr (+1) -< _9
    _5 <- iPre 1.8313682374659503 -< _16
    _6 <- iPre 1.462947185144269 -< _19
    _7 <- iPre 2.2026842245833165 -< _8
    _8 <- iPre 2.467630666925587 -< _12
  _20 <- FRP.Yampa.arr (+1) -< _10
  _21 <- FRP.Yampa.arr (+1) -< _18
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _20)
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _6)
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _23)
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _21)
  _26 <- FRP.Yampa.arr (+1) -< _5
  _27 <- iPre 1.9961283335291593 -< _26
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _27)
  _29 <- iPre 2.576136050177147 -< _28
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr21 (*) -< {_0, _1}
  _3 <- arr11 (+1) -< _0
  _4 <- pre1 2.8055047138551292 -< _1
  rec
    _9 <- arr11 (+1) -< _0
    _10 <- arr21 (*) -< {_0, _2}
    _11 <- arr21 (*) -< {_7, _8}
    _12 <- arr11 (+1) -< _10
    _13 <- arr11 (+1) -< _4
    _14 <- arr11 (+1) -< _10
    _15 <- arr21 (+) -< {_7, _12}
    _16 <- pre1 2.1911412840156745 -< _14
    _17 <- arr21 (+) -< {_15, _11}
    _18 <- arr21 (*) -< {_16, _13}
    _19 <- arr11 (+1) -< _9
    _5 <- pre1 1.8313682374659503 -< _16
    _6 <- pre1 1.462947185144269 -< _19
    _7 <- pre1 2.2026842245833165 -< _8
    _8 <- pre1 2.467630666925587 -< _12

  _20 <- arr11 (+1) -< _10
  _21 <- arr11 (+1) -< _18
  _22 <- arr21 (-) -< {_17, _20}
  _23 <- arr21 (*) -< {_22, _6}
  _24 <- arr21 (-) -< {_3, _23}
  _25 <- arr21 (+) -< {_24, _21}
  _26 <- arr11 (+1) -< _5
  _27 <- pre1 1.9961283335291593 -< _26
  _28 <- arr21 (*) -< {_25, _27}
  _29 <- pre1 2.576136050177147 -< _28
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15