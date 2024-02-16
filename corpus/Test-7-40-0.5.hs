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
    _3 <- FRP.Yampa.arr (+1) -< _2
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _1)
    _6 <- FRP.Yampa.arr (+1) -< _2
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
    _8 <- iPre 2.977726399229159 -< _7
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- FRP.Yampa.arr (+1) -< _9
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _2)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _10)
    _13 <- iPre 0.10602006665152744 -< _7
    _14 <- FRP.Yampa.arr (+1) -< _6
    _15 <- FRP.Yampa.arr (+1) -< _13
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _4)
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _0)
    _18 <- iPre 6.738426182915698e-2 -< _12
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _3)
    _20 <- FRP.Yampa.arr (+1) -< _7
    _21 <- FRP.Yampa.arr (+1) -< _5
    _2 <- iPre 2.935642997188617 -< _17
  _22 <- iPre 0.9306664805178366 -< _3
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _15)
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _1)
  _25 <- iPre 1.2205264717268696 -< _10
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _23)
  _27 <- FRP.Yampa.arr (+1) -< _26
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _14)
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _28)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _29)
  _31 <- FRP.Yampa.arr (+1) -< _18
  _32 <- iPre 2.3297185450128604 -< _31
  _33 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _32)
  _34 <- FRP.Yampa.arr (uncurry (+)) -< (_33, _30)
  _35 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _27)
  _36 <- iPre 2.4408024825187518 -< _35
  _37 <- FRP.Yampa.arr (+1) -< _36
  _38 <- FRP.Yampa.arr (uncurry (-)) -< (_34, _37)
  _39 <- FRP.Yampa.arr (+1) -< _38
  _40 <- iPre 2.68342724170709 -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  rec
    _3 <- arr11 (+1) -< _2
    _4 <- arr11 (+1) -< _1
    _5 <- arr21 (+) -< {_2, _1}
    _6 <- arr11 (+1) -< _2
    _7 <- arr21 (-) -< {_1, _3}
    _8 <- pre1 2.977726399229159 -< _7
    _9 <- arr11 (+1) -< _6
    _10 <- arr11 (+1) -< _9
    _11 <- arr21 (-) -< {_5, _2}
    _12 <- arr21 (-) -< {_10, _10}
    _13 <- pre1 0.10602006665152744 -< _7
    _14 <- arr11 (+1) -< _6
    _15 <- arr11 (+1) -< _13
    _16 <- arr21 (-) -< {_7, _4}
    _17 <- arr21 (*) -< {_16, _0}
    _18 <- pre1 6.738426182915698e-2 -< _12
    _19 <- arr21 (+) -< {_15, _3}
    _20 <- arr11 (+1) -< _7
    _21 <- arr11 (+1) -< _5
    _2 <- pre1 2.935642997188617 -< _17

  _22 <- pre1 0.9306664805178366 -< _3
  _23 <- arr21 (*) -< {_20, _15}
  _24 <- arr21 (+) -< {_19, _1}
  _25 <- pre1 1.2205264717268696 -< _10
  _26 <- arr21 (*) -< {_21, _23}
  _27 <- arr11 (+1) -< _26
  _28 <- arr21 (*) -< {_11, _14}
  _29 <- arr21 (*) -< {_24, _28}
  _30 <- arr21 (-) -< {_22, _29}
  _31 <- arr11 (+1) -< _18
  _32 <- pre1 2.3297185450128604 -< _31
  _33 <- arr21 (*) -< {_25, _32}
  _34 <- arr21 (+) -< {_33, _30}
  _35 <- arr21 (+) -< {_8, _27}
  _36 <- pre1 2.4408024825187518 -< _35
  _37 <- arr11 (+1) -< _36
  _38 <- arr21 (-) -< {_34, _37}
  _39 <- arr11 (+1) -< _38
  _40 <- pre1 2.68342724170709 -< _39
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20