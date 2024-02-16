{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _3 <- iPre 0.27950758936740616 -< _0
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _5 <- iPre 0.1120531044338536 -< _0
    _6 <- iPre 1.2448486301893151 -< _2
    _7 <- iPre 0.6177985994059811 -< _0
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _3)
    _10 <- FRP.Yampa.arr (+1) -< _8
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _8)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _10)
    _13 <- iPre 2.9380552876142842 -< _0
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _7)
    _15 <- FRP.Yampa.arr (+1) -< _1
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _14)
    _17 <- iPre 0.45325626110657624 -< _11
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _10)
    _19 <- FRP.Yampa.arr (+1) -< _7
    _20 <- iPre 0.9452747122775528 -< _14
    _21 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _13)
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _19)
    _23 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _9)
    _24 <- FRP.Yampa.arr (+1) -< _4
    _25 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _12)
    _26 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _16)
    _27 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _22)
    _28 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _27)
    _29 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _26)
    _30 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _29)
    _1 <- iPre 0.4231326715902733 -< _17
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_0, _0}
    _3 <- pre1 0.27950758936740616 -< _0
    _4 <- arr21 (-) -< {_0, _0}
    _5 <- pre1 0.1120531044338536 -< _0
    _6 <- pre1 1.2448486301893151 -< _2
    _7 <- pre1 0.6177985994059811 -< _0
    _8 <- arr21 (+) -< {_2, _0}
    _9 <- arr21 (+) -< {_0, _3}
    _10 <- arr11 (+1) -< _8
    _11 <- arr21 (+) -< {_5, _8}
    _12 <- arr21 (*) -< {_2, _10}
    _13 <- pre1 2.9380552876142842 -< _0
    _14 <- arr21 (*) -< {_0, _7}
    _15 <- arr11 (+1) -< _1
    _16 <- arr21 (-) -< {_8, _14}
    _17 <- pre1 0.45325626110657624 -< _11
    _18 <- arr21 (*) -< {_0, _10}
    _19 <- arr11 (+1) -< _7
    _20 <- pre1 0.9452747122775528 -< _14
    _21 <- arr21 (-) -< {_15, _13}
    _22 <- arr21 (+) -< {_20, _19}
    _23 <- arr21 (-) -< {_21, _9}
    _24 <- arr11 (+1) -< _4
    _25 <- arr21 (*) -< {_6, _12}
    _26 <- arr21 (-) -< {_18, _16}
    _27 <- arr21 (+) -< {_23, _22}
    _28 <- arr21 (-) -< {_25, _27}
    _29 <- arr21 (*) -< {_28, _26}
    _30 <- arr21 (+) -< {_24, _29}
    _1 <- pre1 0.4231326715902733 -< _17

  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30