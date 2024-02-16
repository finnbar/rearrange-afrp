{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (+1) -< _0
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
  rec
    _7 <- FRP.Yampa.arr (+1) -< _5
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- FRP.Yampa.arr (+1) -< _3
    _10 <- iPre 2.978330368295188 -< _7
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _10)
    _12 <- FRP.Yampa.arr (+1) -< _10
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _6)
    _14 <- iPre 2.6395394999237367 -< _2
    _15 <- iPre 0.2755511651787338 -< _6
    _16 <- FRP.Yampa.arr (+1) -< _15
    _17 <- FRP.Yampa.arr (+1) -< _14
    _18 <- FRP.Yampa.arr (+1) -< _4
    _19 <- iPre 1.6121788628991178 -< _16
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _12)
    _21 <- FRP.Yampa.arr (+1) -< _9
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _20)
    _23 <- iPre 0.3852264046891046 -< _19
    _24 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _18)
    _25 <- FRP.Yampa.arr (+1) -< _10
    _6 <- iPre 1.1447460423459945 -< _17
  _26 <- iPre 2.845995997792519 -< _21
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _25)
  _28 <- iPre 0.28192057271190646 -< _23
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _8)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _29)
  _31 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _24)
  _32 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _31)
  _33 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _32)
  _34 <- FRP.Yampa.arr (+1) -< _33
  _35 <- FRP.Yampa.arr (+1) -< _34
  _36 <- iPre 2.7026369594546527 -< _35
  _37 <- FRP.Yampa.arr (+1) -< _36
  _38 <- iPre 2.756004226791226 -< _37
  _39 <- FRP.Yampa.arr (+1) -< _38
  _40 <- iPre 2.1679382523409196 -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (*) -< {_1, _0}
  _3 <- arr11 (+1) -< _2
  _4 <- arr11 (+1) -< _0
  _5 <- arr21 (-) -< {_0, _1}
  rec
    _7 <- arr11 (+1) -< _5
    _8 <- arr11 (+1) -< _7
    _9 <- arr11 (+1) -< _3
    _10 <- pre1 2.978330368295188 -< _7
    _11 <- arr21 (*) -< {_0, _10}
    _12 <- arr11 (+1) -< _10
    _13 <- arr21 (*) -< {_11, _6}
    _14 <- pre1 2.6395394999237367 -< _2
    _15 <- pre1 0.2755511651787338 -< _6
    _16 <- arr11 (+1) -< _15
    _17 <- arr11 (+1) -< _14
    _18 <- arr11 (+1) -< _4
    _19 <- pre1 1.6121788628991178 -< _16
    _20 <- arr21 (-) -< {_7, _12}
    _21 <- arr11 (+1) -< _9
    _22 <- arr21 (+) -< {_16, _20}
    _23 <- pre1 0.3852264046891046 -< _19
    _24 <- arr21 (*) -< {_0, _18}
    _25 <- arr11 (+1) -< _10
    _6 <- pre1 1.1447460423459945 -< _17

  _26 <- pre1 2.845995997792519 -< _21
  _27 <- arr21 (+) -< {_13, _25}
  _28 <- pre1 0.28192057271190646 -< _23
  _29 <- arr21 (+) -< {_26, _8}
  _30 <- arr21 (*) -< {_22, _29}
  _31 <- arr21 (+) -< {_30, _24}
  _32 <- arr21 (*) -< {_27, _31}
  _33 <- arr21 (+) -< {_28, _32}
  _34 <- arr11 (+1) -< _33
  _35 <- arr11 (+1) -< _34
  _36 <- pre1 2.7026369594546527 -< _35
  _37 <- arr11 (+1) -< _36
  _38 <- pre1 2.756004226791226 -< _37
  _39 <- arr11 (+1) -< _38
  _40 <- pre1 2.1679382523409196 -< _39
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20