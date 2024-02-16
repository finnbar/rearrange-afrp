{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- iPre 2.983143623346089 -< _1
  rec
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _4)
    _6 <- iPre 1.578237535414696 -< _4
    _3 <- iPre 0.2881742052596856 -< _5
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (+1) -< _4
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _8)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _9)
  _11 <- iPre 1.015296513276312 -< _3
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _10)
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _12)
  _14 <- FRP.Yampa.arr (+1) -< _4
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _2)
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _7)
  _17 <- iPre 3.0351133311866243 -< _2
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _17)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _13)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _15)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- pre1 2.983143623346089 -< _1
  rec
    _4 <- arr21 (*) -< {_1, _2}
    _5 <- arr21 (-) -< {_3, _4}
    _6 <- pre1 1.578237535414696 -< _4
    _3 <- pre1 0.2881742052596856 -< _5

  _7 <- arr11 (+1) -< _6
  _8 <- arr11 (+1) -< _4
  _9 <- arr21 (-) -< {_7, _8}
  _10 <- arr21 (+) -< {_4, _9}
  _11 <- pre1 1.015296513276312 -< _3
  _12 <- arr21 (-) -< {_6, _10}
  _13 <- arr21 (-) -< {_11, _12}
  _14 <- arr11 (+1) -< _4
  _15 <- arr21 (-) -< {_12, _2}
  _16 <- arr21 (-) -< {_14, _7}
  _17 <- pre1 3.0351133311866243 -< _2
  _18 <- arr21 (+) -< {_16, _17}
  _19 <- arr21 (+) -< {_18, _13}
  _20 <- arr21 (+) -< {_19, _15}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4