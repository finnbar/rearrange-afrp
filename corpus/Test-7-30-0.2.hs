{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _5 <- iPre 2.529485221264275 -< _0
    _6 <- iPre 1.2696095930835054 -< _1
    _1 <- iPre 1.260290973570836 -< _4
    _2 <- iPre 0.9786551029884566 -< _4
    _3 <- iPre 2.327175308151773 -< _0
  _7 <- iPre 1.0137871207485945 -< _2
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _6)
  _9 <- FRP.Yampa.arr (+1) -< _3
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _3)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _2)
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _7)
  _13 <- FRP.Yampa.arr (+1) -< _0
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _8)
  _15 <- FRP.Yampa.arr (+1) -< _13
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _10)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _12)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _9)
  _19 <- iPre 1.19537285784104 -< _8
  _20 <- FRP.Yampa.arr (+1) -< _7
  _21 <- FRP.Yampa.arr (+1) -< _15
  _22 <- FRP.Yampa.arr (+1) -< _18
  _23 <- iPre 0.5584935307508693 -< _20
  _24 <- FRP.Yampa.arr (+1) -< _21
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _22)
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _19)
  _27 <- FRP.Yampa.arr (+1) -< _16
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _27)
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _28)
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr21 (+) -< {_0, _0}
    _5 <- pre1 2.529485221264275 -< _0
    _6 <- pre1 1.2696095930835054 -< _1
    _1 <- pre1 1.260290973570836 -< _4
    _2 <- pre1 0.9786551029884566 -< _4
    _3 <- pre1 2.327175308151773 -< _0

  _7 <- pre1 1.0137871207485945 -< _2
  _8 <- arr21 (+) -< {_7, _6}
  _9 <- arr11 (+1) -< _3
  _10 <- arr21 (-) -< {_5, _3}
  _11 <- arr21 (-) -< {_5, _2}
  _12 <- arr21 (*) -< {_11, _7}
  _13 <- arr11 (+1) -< _0
  _14 <- arr21 (*) -< {_8, _8}
  _15 <- arr11 (+1) -< _13
  _16 <- arr21 (*) -< {_9, _10}
  _17 <- arr21 (+) -< {_14, _12}
  _18 <- arr21 (*) -< {_17, _9}
  _19 <- pre1 1.19537285784104 -< _8
  _20 <- arr11 (+1) -< _7
  _21 <- arr11 (+1) -< _15
  _22 <- arr11 (+1) -< _18
  _23 <- pre1 0.5584935307508693 -< _20
  _24 <- arr11 (+1) -< _21
  _25 <- arr21 (+) -< {_23, _22}
  _26 <- arr21 (+) -< {_25, _19}
  _27 <- arr11 (+1) -< _16
  _28 <- arr21 (+) -< {_26, _27}
  _29 <- arr21 (-) -< {_24, _28}
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6