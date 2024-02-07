{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _3)
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
    _6 <- FRP.Yampa.arr (+1) -< _1
    _7 <- FRP.Yampa.arr (+1) -< _5
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- iPre 0.6588624343987907 -< _2
    _10 <- FRP.Yampa.arr (+1) -< _2
    _11 <- iPre 0.15779492408767432 -< _9
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _6)
    _13 <- iPre 0.4670270794302122 -< _4
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _12)
    _15 <- iPre 2.0956877646446195 -< _14
    _1 <- iPre 1.3919725078993006 -< _15
    _2 <- iPre 0.9214654734582892 -< _9
    _3 <- iPre 0.9988395358048926 -< _10
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _7)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _8)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _4)
  _19 <- iPre 0.2902968098830716 -< _10
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _18)
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _0)
  _22 <- FRP.Yampa.arr (+1) -< _21
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _16)
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _23)
  _25 <- iPre 3.0311950989519816 -< _24
  _26 <- iPre 1.9209962871388306 -< _25
  _27 <- iPre 0.4538617325044785 -< _26
  _28 <- iPre 1.0819828718178 -< _27
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 2.1430296893876943 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr21 (-) -< {_0, _3}
    _5 <- arr21 (*) -< {_2, _1}
    _6 <- arr11 (+1) -< _1
    _7 <- arr11 (+1) -< _5
    _8 <- arr11 (+1) -< _7
    _9 <- pre1 0.6588624343987907 -< _2
    _10 <- arr11 (+1) -< _2
    _11 <- pre1 0.15779492408767432 -< _9
    _12 <- arr21 (*) -< {_10, _6}
    _13 <- pre1 0.4670270794302122 -< _4
    _14 <- arr21 (*) -< {_8, _12}
    _15 <- pre1 2.0956877646446195 -< _14
    _1 <- pre1 1.3919725078993006 -< _15
    _2 <- pre1 0.9214654734582892 -< _9
    _3 <- pre1 0.9988395358048926 -< _10

  _16 <- arr21 (+) -< {_11, _7}
  _17 <- arr21 (-) -< {_8, _8}
  _18 <- arr21 (*) -< {_17, _4}
  _19 <- pre1 0.2902968098830716 -< _10
  _20 <- arr21 (-) -< {_19, _18}
  _21 <- arr21 (-) -< {_20, _0}
  _22 <- arr11 (+1) -< _21
  _23 <- arr21 (-) -< {_13, _16}
  _24 <- arr21 (*) -< {_22, _23}
  _25 <- pre1 3.0311950989519816 -< _24
  _26 <- pre1 1.9209962871388306 -< _25
  _27 <- pre1 0.4538617325044785 -< _26
  _28 <- pre1 1.0819828718178 -< _27
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 2.1430296893876943 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15