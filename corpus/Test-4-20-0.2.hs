{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _0)
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
  rec
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _2)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _6)
    _5 <- iPre 0.2191307090962634 -< _2
    _6 <- iPre 1.9675006130009123 -< _0
  _9 <- iPre 0.6623428179574193 -< _8
  _10 <- FRP.Yampa.arr (+1) -< _1
  _11 <- FRP.Yampa.arr (+1) -< _5
  _12 <- FRP.Yampa.arr (+1) -< _9
  _13 <- FRP.Yampa.arr (+1) -< _12
  _14 <- iPre 2.855853288784116 -< _13
  _15 <- iPre 2.6333596410470146 -< _10
  _16 <- iPre 1.870445429040233 -< _11
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _16)
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _18)
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr11 (+1) -< _0
  _3 <- arr21 (-) -< {_2, _0}
  _4 <- arr21 (-) -< {_2, _3}
  rec
    _7 <- arr21 (-) -< {_4, _2}
    _8 <- arr21 (+) -< {_7, _6}
    _5 <- pre1 0.2191307090962634 -< _2
    _6 <- pre1 1.9675006130009123 -< _0

  _9 <- pre1 0.6623428179574193 -< _8
  _10 <- arr11 (+1) -< _1
  _11 <- arr11 (+1) -< _5
  _12 <- arr11 (+1) -< _9
  _13 <- arr11 (+1) -< _12
  _14 <- pre1 2.855853288784116 -< _13
  _15 <- pre1 2.6333596410470146 -< _10
  _16 <- pre1 1.870445429040233 -< _11
  _17 <- arr21 (*) -< {_14, _16}
  _18 <- arr11 (+1) -< _17
  _19 <- arr21 (*) -< {_15, _18}
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4