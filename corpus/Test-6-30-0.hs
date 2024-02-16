{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (+1) -< _3
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _3)
  _6 <- FRP.Yampa.arr (+1) -< _5
  _7 <- FRP.Yampa.arr (+1) -< _5
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _6)
  _9 <- FRP.Yampa.arr (+1) -< _1
  _10 <- iPre 2.120585233974406 -< _8
  _11 <- FRP.Yampa.arr (+1) -< _7
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _10)
  _13 <- iPre 1.4191911102849968e-2 -< _9
  _14 <- iPre 0.863874388164986 -< _4
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _14)
  _16 <- iPre 2.971793868405142 -< _15
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _12)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _4)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _17)
  _20 <- iPre 0.6775324290099785 -< _18
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _19)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _13)
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _22)
  _24 <- FRP.Yampa.arr (+1) -< _23
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _5)
  _26 <- FRP.Yampa.arr (+1) -< _24
  _27 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _7)
  _28 <- FRP.Yampa.arr (+1) -< _25
  _29 <- iPre 1.9770349682591 -< _27
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (+) -< {_0, _0}
  _3 <- arr11 (+1) -< _2
  _4 <- arr11 (+1) -< _3
  _5 <- arr21 (-) -< {_4, _3}
  _6 <- arr11 (+1) -< _5
  _7 <- arr11 (+1) -< _5
  _8 <- arr21 (-) -< {_7, _6}
  _9 <- arr11 (+1) -< _1
  _10 <- pre1 2.120585233974406 -< _8
  _11 <- arr11 (+1) -< _7
  _12 <- arr21 (*) -< {_11, _10}
  _13 <- pre1 1.4191911102849968e-2 -< _9
  _14 <- pre1 0.863874388164986 -< _4
  _15 <- arr21 (+) -< {_9, _14}
  _16 <- pre1 2.971793868405142 -< _15
  _17 <- arr21 (-) -< {_16, _12}
  _18 <- arr21 (*) -< {_3, _4}
  _19 <- arr21 (-) -< {_11, _17}
  _20 <- pre1 0.6775324290099785 -< _18
  _21 <- arr21 (-) -< {_20, _19}
  _22 <- arr21 (-) -< {_6, _13}
  _23 <- arr21 (-) -< {_21, _22}
  _24 <- arr11 (+1) -< _23
  _25 <- arr21 (+) -< {_13, _5}
  _26 <- arr11 (+1) -< _24
  _27 <- arr21 (-) -< {_26, _7}
  _28 <- arr11 (+1) -< _25
  _29 <- pre1 1.9770349682591 -< _27
  _30 <- arr21 (-) -< {_28, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0