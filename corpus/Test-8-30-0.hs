{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (+1) -< _1
  _4 <- FRP.Yampa.arr (+1) -< _2
  _5 <- FRP.Yampa.arr (+1) -< _3
  _6 <- iPre 2.5785896454735324 -< _4
  _7 <- FRP.Yampa.arr (+1) -< _5
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _7)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _7)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _5)
  _11 <- iPre 0.4140256499800323 -< _8
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _7)
  _13 <- FRP.Yampa.arr (+1) -< _11
  _14 <- iPre 1.201203059594217 -< _10
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _14)
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _15)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _3)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _16)
  _19 <- iPre 2.313295276620217 -< _9
  _20 <- iPre 1.47551588879588 -< _19
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _8)
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _9)
  _23 <- iPre 2.031706640705707 -< _21
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _20)
  _25 <- FRP.Yampa.arr (+1) -< _10
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _8)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _25)
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _10)
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _27)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- arr11 (+1) -< _1
  _4 <- arr11 (+1) -< _2
  _5 <- arr11 (+1) -< _3
  _6 <- pre1 2.5785896454735324 -< _4
  _7 <- arr11 (+1) -< _5
  _8 <- arr21 (-) -< {_6, _7}
  _9 <- arr21 (+) -< {_8, _7}
  _10 <- arr21 (-) -< {_9, _5}
  _11 <- pre1 0.4140256499800323 -< _8
  _12 <- arr21 (+) -< {_10, _7}
  _13 <- arr11 (+1) -< _11
  _14 <- pre1 1.201203059594217 -< _10
  _15 <- arr21 (-) -< {_12, _14}
  _16 <- arr21 (-) -< {_7, _15}
  _17 <- arr21 (+) -< {_13, _3}
  _18 <- arr21 (+) -< {_17, _16}
  _19 <- pre1 2.313295276620217 -< _9
  _20 <- pre1 1.47551588879588 -< _19
  _21 <- arr21 (*) -< {_18, _8}
  _22 <- arr21 (*) -< {_20, _9}
  _23 <- pre1 2.031706640705707 -< _21
  _24 <- arr21 (-) -< {_6, _20}
  _25 <- arr11 (+1) -< _10
  _26 <- arr21 (-) -< {_23, _8}
  _27 <- arr21 (+) -< {_24, _25}
  _28 <- arr21 (-) -< {_22, _10}
  _29 <- arr21 (-) -< {_26, _27}
  _30 <- arr21 (*) -< {_28, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0