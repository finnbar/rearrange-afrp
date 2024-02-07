{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _6 <- iPre 1.778158319696647 -< _5
    _7 <- FRP.Yampa.arr (+1) -< _2
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _2)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _5)
    _11 <- FRP.Yampa.arr (+1) -< _4
    _12 <- iPre 1.8116418975036388 -< _7
    _13 <- FRP.Yampa.arr (+1) -< _6
    _14 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _8)
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _1)
    _1 <- iPre 1.7717069436378394 -< _15
    _2 <- iPre 1.8631873698030226 -< _8
    _3 <- iPre 0.14075207402965376 -< _8
    _4 <- iPre 0.23703517933302576 -< _12
    _5 <- iPre 2.3795788915018017 -< _9
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _2)
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _16)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _17)
  _19 <- FRP.Yampa.arr (+1) -< _11
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _13)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _20)
  _22 <- FRP.Yampa.arr (+1) -< _3
  _23 <- FRP.Yampa.arr (+1) -< _22
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _21)
  _25 <- FRP.Yampa.arr (+1) -< _23
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _24)
  _27 <- iPre 0.5799279109604057 -< _26
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_27, _25)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 2.0038975189256067 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _6 <- pre1 1.778158319696647 -< _5
    _7 <- arr11 (+1) -< _2
    _8 <- arr11 (+1) -< _7
    _9 <- arr21 (+) -< {_5, _2}
    _10 <- arr21 (+) -< {_1, _5}
    _11 <- arr11 (+1) -< _4
    _12 <- pre1 1.8116418975036388 -< _7
    _13 <- arr11 (+1) -< _6
    _14 <- arr21 (-) -< {_2, _8}
    _15 <- arr21 (*) -< {_6, _1}
    _1 <- pre1 1.7717069436378394 -< _15
    _2 <- pre1 1.8631873698030226 -< _8
    _3 <- pre1 0.14075207402965376 -< _8
    _4 <- pre1 0.23703517933302576 -< _12
    _5 <- pre1 2.3795788915018017 -< _9

  _16 <- arr21 (*) -< {_6, _2}
  _17 <- arr21 (*) -< {_14, _16}
  _18 <- arr21 (+) -< {_9, _17}
  _19 <- arr11 (+1) -< _11
  _20 <- arr21 (-) -< {_18, _13}
  _21 <- arr21 (+) -< {_10, _20}
  _22 <- arr11 (+1) -< _3
  _23 <- arr11 (+1) -< _22
  _24 <- arr21 (+) -< {_19, _21}
  _25 <- arr11 (+1) -< _23
  _26 <- arr21 (-) -< {_0, _24}
  _27 <- pre1 0.5799279109604057 -< _26
  _28 <- arr21 (-) -< {_27, _25}
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 2.0038975189256067 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15