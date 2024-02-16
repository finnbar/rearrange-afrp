{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- iPre 0.22760966426758114 -< _1
  _4 <- FRP.Yampa.arr (+1) -< _0
  _5 <- iPre 3.0251839092721426 -< _0
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _7 <- FRP.Yampa.arr (+1) -< _1
  _8 <- iPre 0.5952840933138952 -< _4
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _0)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _0)
  _11 <- FRP.Yampa.arr (+1) -< _7
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _7)
  _13 <- FRP.Yampa.arr (+1) -< _5
  _14 <- FRP.Yampa.arr (+1) -< _2
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _11)
  _16 <- iPre 0.462684570823751 -< _4
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _5)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _4)
  _19 <- FRP.Yampa.arr (+1) -< _9
  _20 <- FRP.Yampa.arr (+1) -< _1
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _6)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _14)
  _23 <- FRP.Yampa.arr (+1) -< _18
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _17)
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _19)
  _26 <- FRP.Yampa.arr (+1) -< _12
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _25)
  _28 <- FRP.Yampa.arr (+1) -< _23
  _29 <- FRP.Yampa.arr (+1) -< _26
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _22)
  _31 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _21)
  _32 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _29)
  _33 <- FRP.Yampa.arr (+1) -< _32
  _34 <- FRP.Yampa.arr (uncurry (*)) -< (_31, _33)
  _35 <- iPre 0.8829007538047501 -< _34
  _36 <- FRP.Yampa.arr (+1) -< _35
  _37 <- FRP.Yampa.arr (+1) -< _36
  _38 <- iPre 2.1409102752950897 -< _37
  _39 <- FRP.Yampa.arr (+1) -< _38
  _40 <- FRP.Yampa.arr (+1) -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  _3 <- pre1 0.22760966426758114 -< _1
  _4 <- arr11 (+1) -< _0
  _5 <- pre1 3.0251839092721426 -< _0
  _6 <- arr21 (-) -< {_0, _0}
  _7 <- arr11 (+1) -< _1
  _8 <- pre1 0.5952840933138952 -< _4
  _9 <- arr21 (*) -< {_4, _0}
  _10 <- arr21 (-) -< {_8, _0}
  _11 <- arr11 (+1) -< _7
  _12 <- arr21 (*) -< {_8, _7}
  _13 <- arr11 (+1) -< _5
  _14 <- arr11 (+1) -< _2
  _15 <- arr21 (+) -< {_7, _11}
  _16 <- pre1 0.462684570823751 -< _4
  _17 <- arr21 (+) -< {_15, _5}
  _18 <- arr21 (*) -< {_16, _4}
  _19 <- arr11 (+1) -< _9
  _20 <- arr11 (+1) -< _1
  _21 <- arr21 (-) -< {_3, _6}
  _22 <- arr21 (-) -< {_10, _14}
  _23 <- arr11 (+1) -< _18
  _24 <- arr21 (+) -< {_13, _17}
  _25 <- arr21 (*) -< {_20, _19}
  _26 <- arr11 (+1) -< _12
  _27 <- arr21 (*) -< {_24, _25}
  _28 <- arr11 (+1) -< _23
  _29 <- arr11 (+1) -< _26
  _30 <- arr21 (*) -< {_27, _22}
  _31 <- arr21 (+) -< {_28, _21}
  _32 <- arr21 (+) -< {_30, _29}
  _33 <- arr11 (+1) -< _32
  _34 <- arr21 (*) -< {_31, _33}
  _35 <- pre1 0.8829007538047501 -< _34
  _36 <- arr11 (+1) -< _35
  _37 <- arr11 (+1) -< _36
  _38 <- pre1 2.1409102752950897 -< _37
  _39 <- arr11 (+1) -< _38
  _40 <- arr11 (+1) -< _39
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0