{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _0)
    _5 <- iPre 1.9589749860735879 -< _1
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _5)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _6)
    _8 <- FRP.Yampa.arr (+1) -< _6
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _8)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _4)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _4)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _6)
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _1)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _1)
    _15 <- FRP.Yampa.arr (+1) -< _4
    _1 <- iPre 1.5453720184997946 -< _9
    _2 <- iPre 1.3729058989690364 -< _15
    _3 <- iPre 1.102641163132368 -< _6
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _5)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _15)
  _18 <- iPre 2.2414778544996077 -< _15
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _17)
  _20 <- iPre 2.8646033268528566 -< _7
  _21 <- FRP.Yampa.arr (+1) -< _16
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _20)
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _18)
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _19)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _22)
  _26 <- FRP.Yampa.arr (+1) -< _23
  _27 <- iPre 0.8704188934305238 -< _26
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _27)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr21 (-) -< {_2, _0}
    _5 <- pre1 1.9589749860735879 -< _1
    _6 <- arr21 (*) -< {_3, _5}
    _7 <- arr21 (*) -< {_0, _6}
    _8 <- arr11 (+1) -< _6
    _9 <- arr21 (+) -< {_4, _8}
    _10 <- arr21 (+) -< {_0, _4}
    _11 <- arr21 (*) -< {_8, _4}
    _12 <- arr21 (-) -< {_9, _6}
    _13 <- arr21 (-) -< {_11, _1}
    _14 <- arr21 (+) -< {_6, _1}
    _15 <- arr11 (+1) -< _4
    _1 <- pre1 1.5453720184997946 -< _9
    _2 <- pre1 1.3729058989690364 -< _15
    _3 <- pre1 1.102641163132368 -< _6

  _16 <- arr21 (+) -< {_14, _5}
  _17 <- arr21 (-) -< {_9, _15}
  _18 <- pre1 2.2414778544996077 -< _15
  _19 <- arr21 (*) -< {_13, _17}
  _20 <- pre1 2.8646033268528566 -< _7
  _21 <- arr11 (+1) -< _16
  _22 <- arr21 (+) -< {_10, _20}
  _23 <- arr21 (+) -< {_12, _18}
  _24 <- arr21 (*) -< {_21, _19}
  _25 <- arr21 (-) -< {_24, _22}
  _26 <- arr11 (+1) -< _23
  _27 <- pre1 0.8704188934305238 -< _26
  _28 <- arr21 (-) -< {_25, _27}
  _29 <- arr11 (+1) -< _28
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15