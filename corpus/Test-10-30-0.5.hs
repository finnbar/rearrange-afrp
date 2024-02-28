{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 2.2424097411275863 -< _0
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _0)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _2)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _3)
    _8 <- FRP.Yampa.arr (+1) -< _5
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- iPre 1.0285342444844388 -< _9
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _10)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _4)
    _13 <- FRP.Yampa.arr (+1) -< _11
    _14 <- FRP.Yampa.arr (+1) -< _13
    _15 <- FRP.Yampa.arr (+1) -< _4
    _1 <- iPre 0.1458933430565078 -< _4
  _16 <- iPre 1.30124743185608 -< _12
  _17 <- FRP.Yampa.arr (+1) -< _1
  _18 <- FRP.Yampa.arr (+1) -< _15
  _19 <- FRP.Yampa.arr (+1) -< _7
  _20 <- iPre 0.863706488050343 -< _18
  _21 <- iPre 1.1091736144378146 -< _17
  _22 <- FRP.Yampa.arr (+1) -< _16
  _23 <- FRP.Yampa.arr (+1) -< _19
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _21)
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _24)
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _23)
  _27 <- FRP.Yampa.arr (+1) -< _22
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _26)
  _30 <- iPre 0.967911600372608 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 2.2424097411275863 -< _0
    _3 <- arr21 (+) -< {_1, _1}
    _4 <- arr21 (*) -< {_3, _2}
    _5 <- arr21 (+) -< {_4, _0}
    _6 <- arr21 (-) -< {_4, _2}
    _7 <- arr21 (*) -< {_6, _3}
    _8 <- arr11 (+1) -< _5
    _9 <- arr11 (+1) -< _8
    _10 <- pre1 1.0285342444844388 -< _9
    _11 <- arr21 (-) -< {_10, _10}
    _12 <- arr21 (*) -< {_3, _4}
    _13 <- arr11 (+1) -< _11
    _14 <- arr11 (+1) -< _13
    _15 <- arr11 (+1) -< _4
    _1 <- pre1 0.1458933430565078 -< _4

  _16 <- pre1 1.30124743185608 -< _12
  _17 <- arr11 (+1) -< _1
  _18 <- arr11 (+1) -< _15
  _19 <- arr11 (+1) -< _7
  _20 <- pre1 0.863706488050343 -< _18
  _21 <- pre1 1.1091736144378146 -< _17
  _22 <- arr11 (+1) -< _16
  _23 <- arr11 (+1) -< _19
  _24 <- arr21 (-) -< {_20, _21}
  _25 <- arr21 (+) -< {_14, _24}
  _26 <- arr21 (+) -< {_25, _23}
  _27 <- arr11 (+1) -< _22
  _28 <- arr11 (+1) -< _27
  _29 <- arr21 (+) -< {_28, _26}
  _30 <- pre1 0.967911600372608 -< _29
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15