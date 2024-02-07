{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _0)
  _4 <- iPre 2.1229292069638546 -< _3
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _4)
  _6 <- iPre 0.15838401937141383 -< _5
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _5)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _4)
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (+1) -< _3
  _11 <- iPre 2.918447556426919 -< _2
  _12 <- iPre 0.9789813967640425 -< _6
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _12)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _6)
  _15 <- iPre 1.0452511769679822 -< _7
  _16 <- iPre 2.518570827197636 -< _12
  _17 <- iPre 5.419769924248262e-2 -< _1
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _17)
  _19 <- iPre 1.1763351976654015 -< _15
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _19)
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _17)
  _22 <- iPre 2.280516459544466 -< _20
  _23 <- FRP.Yampa.arr (+1) -< _13
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _14)
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _9)
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _21)
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _22)
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _27)
  _29 <- iPre 2.433777262058876 -< _28
  _30 <- iPre 2.8738835283210893 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (*) -< {_1, _1}
  _3 <- arr21 (*) -< {_2, _0}
  _4 <- pre1 2.1229292069638546 -< _3
  _5 <- arr21 (*) -< {_4, _4}
  _6 <- pre1 0.15838401937141383 -< _5
  _7 <- arr21 (*) -< {_0, _5}
  _8 <- arr21 (*) -< {_4, _4}
  _9 <- arr11 (+1) -< _8
  _10 <- arr11 (+1) -< _3
  _11 <- pre1 2.918447556426919 -< _2
  _12 <- pre1 0.9789813967640425 -< _6
  _13 <- arr21 (-) -< {_4, _12}
  _14 <- arr21 (*) -< {_10, _6}
  _15 <- pre1 1.0452511769679822 -< _7
  _16 <- pre1 2.518570827197636 -< _12
  _17 <- pre1 5.419769924248262e-2 -< _1
  _18 <- arr21 (+) -< {_16, _17}
  _19 <- pre1 1.1763351976654015 -< _15
  _20 <- arr21 (-) -< {_11, _19}
  _21 <- arr21 (-) -< {_5, _17}
  _22 <- pre1 2.280516459544466 -< _20
  _23 <- arr11 (+1) -< _13
  _24 <- arr21 (*) -< {_23, _14}
  _25 <- arr21 (+) -< {_18, _9}
  _26 <- arr21 (*) -< {_25, _21}
  _27 <- arr21 (*) -< {_26, _22}
  _28 <- arr21 (-) -< {_24, _27}
  _29 <- pre1 2.433777262058876 -< _28
  _30 <- pre1 2.8738835283210893 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0