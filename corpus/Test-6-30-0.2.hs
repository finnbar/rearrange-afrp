{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
  _4 <- FRP.Yampa.arr (+1) -< _0
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _1)
  _6 <- FRP.Yampa.arr (+1) -< _4
  _7 <- iPre 2.796618801222681 -< _4
  rec
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _3)
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _7)
    _11 <- FRP.Yampa.arr (+1) -< _6
    _12 <- FRP.Yampa.arr (+1) -< _8
    _13 <- iPre 2.604614911410915 -< _8
    _8 <- iPre 2.085542815219026 -< _12
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _1)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _14)
  _16 <- FRP.Yampa.arr (+1) -< _10
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _9)
  _18 <- FRP.Yampa.arr (+1) -< _5
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _18)
  _20 <- iPre 0.7313735739244143 -< _17
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _17)
  _22 <- iPre 2.4733174036213508 -< _20
  _23 <- FRP.Yampa.arr (+1) -< _15
  _24 <- FRP.Yampa.arr (+1) -< _13
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _6)
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _21)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _24)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _25)
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _22)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr21 (+) -< {_0, _0}
  _3 <- arr21 (+) -< {_1, _2}
  _4 <- arr11 (+1) -< _0
  _5 <- arr21 (-) -< {_4, _1}
  _6 <- arr11 (+1) -< _4
  _7 <- pre1 2.796618801222681 -< _4
  rec
    _9 <- arr21 (+) -< {_4, _3}
    _10 <- arr21 (-) -< {_5, _7}
    _11 <- arr11 (+1) -< _6
    _12 <- arr11 (+1) -< _8
    _13 <- pre1 2.604614911410915 -< _8
    _8 <- pre1 2.085542815219026 -< _12

  _14 <- arr21 (-) -< {_8, _1}
  _15 <- arr21 (+) -< {_11, _14}
  _16 <- arr11 (+1) -< _10
  _17 <- arr21 (*) -< {_13, _9}
  _18 <- arr11 (+1) -< _5
  _19 <- arr21 (+) -< {_16, _18}
  _20 <- pre1 0.7313735739244143 -< _17
  _21 <- arr21 (*) -< {_1, _17}
  _22 <- pre1 2.4733174036213508 -< _20
  _23 <- arr11 (+1) -< _15
  _24 <- arr11 (+1) -< _13
  _25 <- arr21 (+) -< {_19, _6}
  _26 <- arr21 (*) -< {_2, _21}
  _27 <- arr21 (+) -< {_23, _24}
  _28 <- arr21 (*) -< {_26, _25}
  _29 <- arr21 (+) -< {_27, _22}
  _30 <- arr21 (*) -< {_28, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6