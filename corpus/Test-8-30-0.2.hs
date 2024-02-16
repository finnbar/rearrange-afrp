{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.1040670380046709 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- iPre 2.2240658175047057 -< _1
  _4 <- FRP.Yampa.arr (+1) -< _2
  _5 <- FRP.Yampa.arr (+1) -< _2
  rec
    _7 <- iPre 1.3605667523681102 -< _2
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _1)
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _6)
    _6 <- iPre 2.5407859816299414 -< _1
  _12 <- FRP.Yampa.arr (+1) -< _7
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _4)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _8)
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _5)
  _16 <- iPre 0.22975389871650373 -< _15
  _17 <- FRP.Yampa.arr (+1) -< _5
  _18 <- FRP.Yampa.arr (+1) -< _13
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _12)
  _20 <- iPre 2.215681793721964 -< _18
  _21 <- FRP.Yampa.arr (+1) -< _16
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _14)
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _11)
  _24 <- FRP.Yampa.arr (+1) -< _23
  _25 <- FRP.Yampa.arr (+1) -< _21
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _19)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _24)
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _25)
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.1040670380046709 -< _0
  _2 <- arr11 (+1) -< _1
  _3 <- pre1 2.2240658175047057 -< _1
  _4 <- arr11 (+1) -< _2
  _5 <- arr11 (+1) -< _2
  rec
    _7 <- pre1 1.3605667523681102 -< _2
    _8 <- arr21 (-) -< {_2, _3}
    _9 <- arr21 (*) -< {_1, _1}
    _10 <- arr21 (-) -< {_8, _1}
    _11 <- arr21 (+) -< {_10, _6}
    _6 <- pre1 2.5407859816299414 -< _1

  _12 <- arr11 (+1) -< _7
  _13 <- arr21 (*) -< {_7, _4}
  _14 <- arr21 (+) -< {_7, _8}
  _15 <- arr21 (*) -< {_6, _5}
  _16 <- pre1 0.22975389871650373 -< _15
  _17 <- arr11 (+1) -< _5
  _18 <- arr11 (+1) -< _13
  _19 <- arr21 (-) -< {_9, _12}
  _20 <- pre1 2.215681793721964 -< _18
  _21 <- arr11 (+1) -< _16
  _22 <- arr21 (*) -< {_20, _14}
  _23 <- arr21 (+) -< {_22, _11}
  _24 <- arr11 (+1) -< _23
  _25 <- arr11 (+1) -< _21
  _26 <- arr21 (*) -< {_17, _19}
  _27 <- arr21 (+) -< {_26, _24}
  _28 <- arr11 (+1) -< _27
  _29 <- arr21 (-) -< {_28, _25}
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6