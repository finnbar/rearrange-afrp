{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _4)
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _0)
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _8)
    _1 <- iPre 2.0683239659176658 -< _8
    _2 <- iPre 2.040423581779723 -< _5
    _3 <- iPre 1.8506900960681885 -< _10
    _4 <- iPre 1.0447153403542455 -< _1
  _11 <- FRP.Yampa.arr (+1) -< _9
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _11)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _12)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _9)
  _15 <- FRP.Yampa.arr (+1) -< _12
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- FRP.Yampa.arr (+1) -< _15
  _18 <- iPre 1.832958290446489 -< _17
  _19 <- iPre 2.0460395101089497 -< _18
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _16)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _5 <- arr21 (+) -< {_2, _4}
    _6 <- arr21 (+) -< {_1, _2}
    _7 <- arr21 (*) -< {_3, _0}
    _8 <- arr11 (+1) -< _7
    _9 <- arr11 (+1) -< _6
    _10 <- arr21 (*) -< {_8, _8}
    _1 <- pre1 2.0683239659176658 -< _8
    _2 <- pre1 2.040423581779723 -< _5
    _3 <- pre1 1.8506900960681885 -< _10
    _4 <- pre1 1.0447153403542455 -< _1

  _11 <- arr11 (+1) -< _9
  _12 <- arr21 (+) -< {_2, _11}
  _13 <- arr21 (*) -< {_7, _12}
  _14 <- arr21 (*) -< {_13, _9}
  _15 <- arr11 (+1) -< _12
  _16 <- arr11 (+1) -< _14
  _17 <- arr11 (+1) -< _15
  _18 <- pre1 1.832958290446489 -< _17
  _19 <- pre1 2.0460395101089497 -< _18
  _20 <- arr21 (*) -< {_19, _16}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10