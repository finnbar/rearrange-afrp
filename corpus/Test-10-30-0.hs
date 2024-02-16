{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.0502563649234464 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (+1) -< _0
  _4 <- iPre 1.9688236028914756 -< _0
  _5 <- iPre 2.36049172429816 -< _1
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _1)
  _7 <- iPre 1.8253845035656955 -< _4
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _7)
  _9 <- FRP.Yampa.arr (+1) -< _1
  _10 <- FRP.Yampa.arr (+1) -< _8
  _11 <- iPre 0.7883192632128624 -< _5
  _12 <- FRP.Yampa.arr (+1) -< _4
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _3)
  _14 <- iPre 2.685963097902903 -< _3
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _5)
  _16 <- FRP.Yampa.arr (+1) -< _13
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _9)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _12)
  _19 <- FRP.Yampa.arr (+1) -< _18
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _19)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _14)
  _22 <- iPre 0.6769419216148549 -< _11
  _23 <- FRP.Yampa.arr (+1) -< _21
  _24 <- FRP.Yampa.arr (+1) -< _22
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _17)
  _26 <- iPre 0.6863395128061839 -< _23
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _26)
  _28 <- FRP.Yampa.arr (+1) -< _25
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _28)
  _30 <- iPre 1.3386009532087022 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.0502563649234464 -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- arr11 (+1) -< _0
  _4 <- pre1 1.9688236028914756 -< _0
  _5 <- pre1 2.36049172429816 -< _1
  _6 <- arr21 (*) -< {_4, _1}
  _7 <- pre1 1.8253845035656955 -< _4
  _8 <- arr21 (*) -< {_2, _7}
  _9 <- arr11 (+1) -< _1
  _10 <- arr11 (+1) -< _8
  _11 <- pre1 0.7883192632128624 -< _5
  _12 <- arr11 (+1) -< _4
  _13 <- arr21 (*) -< {_1, _3}
  _14 <- pre1 2.685963097902903 -< _3
  _15 <- arr21 (-) -< {_10, _5}
  _16 <- arr11 (+1) -< _13
  _17 <- arr21 (+) -< {_2, _9}
  _18 <- arr21 (+) -< {_15, _12}
  _19 <- arr11 (+1) -< _18
  _20 <- arr21 (*) -< {_6, _19}
  _21 <- arr21 (+) -< {_20, _14}
  _22 <- pre1 0.6769419216148549 -< _11
  _23 <- arr11 (+1) -< _21
  _24 <- arr11 (+1) -< _22
  _25 <- arr21 (+) -< {_16, _17}
  _26 <- pre1 0.6863395128061839 -< _23
  _27 <- arr21 (+) -< {_24, _26}
  _28 <- arr11 (+1) -< _25
  _29 <- arr21 (+) -< {_27, _28}
  _30 <- pre1 1.3386009532087022 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0