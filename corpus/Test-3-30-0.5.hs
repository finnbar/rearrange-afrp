{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.0059126468421333 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  rec
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _3)
    _7 <- FRP.Yampa.arr (+1) -< _5
    _8 <- FRP.Yampa.arr (+1) -< _1
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _8)
    _10 <- FRP.Yampa.arr (+1) -< _4
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _6)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _4)
    _13 <- FRP.Yampa.arr (+1) -< _12
    _14 <- FRP.Yampa.arr (+1) -< _8
    _15 <- FRP.Yampa.arr (+1) -< _9
    _16 <- FRP.Yampa.arr (+1) -< _15
    _17 <- iPre 1.907173939783915 -< _13
    _18 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _7)
    _19 <- iPre 1.9952687228050938 -< _6
    _5 <- iPre 2.313497658532077 -< _1
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _18)
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _19)
  _22 <- FRP.Yampa.arr (+1) -< _16
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _20)
  _24 <- iPre 1.6271132438181042 -< _21
  _25 <- iPre 1.151056276886918 -< _22
  _26 <- iPre 0.737954813193553 -< _25
  _27 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _23)
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _26)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 2.1182494728135204 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.0059126468421333 -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- arr21 (+) -< {_0, _0}
  _4 <- arr21 (*) -< {_0, _0}
  rec
    _6 <- arr21 (+) -< {_3, _3}
    _7 <- arr11 (+1) -< _5
    _8 <- arr11 (+1) -< _1
    _9 <- arr21 (*) -< {_2, _8}
    _10 <- arr11 (+1) -< _4
    _11 <- arr21 (+) -< {_8, _6}
    _12 <- arr21 (*) -< {_10, _4}
    _13 <- arr11 (+1) -< _12
    _14 <- arr11 (+1) -< _8
    _15 <- arr11 (+1) -< _9
    _16 <- arr11 (+1) -< _15
    _17 <- pre1 1.907173939783915 -< _13
    _18 <- arr21 (-) -< {_14, _7}
    _19 <- pre1 1.9952687228050938 -< _6
    _5 <- pre1 2.313497658532077 -< _1

  _20 <- arr21 (+) -< {_3, _18}
  _21 <- arr21 (-) -< {_17, _19}
  _22 <- arr11 (+1) -< _16
  _23 <- arr21 (-) -< {_11, _20}
  _24 <- pre1 1.6271132438181042 -< _21
  _25 <- pre1 1.151056276886918 -< _22
  _26 <- pre1 0.737954813193553 -< _25
  _27 <- arr21 (-) -< {_24, _23}
  _28 <- arr21 (+) -< {_27, _26}
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 2.1182494728135204 -< _29
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15