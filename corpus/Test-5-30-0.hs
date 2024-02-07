{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.8508305790742094 -< _0
  _2 <- iPre 1.079658531511982 -< _1
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _2)
  _5 <- iPre 1.1143925971208914 -< _4
  _6 <- iPre 2.967639801827742 -< _5
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _1)
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _7)
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _8)
  _10 <- FRP.Yampa.arr (+1) -< _2
  _11 <- FRP.Yampa.arr (+1) -< _6
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _11)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _0)
  _14 <- FRP.Yampa.arr (+1) -< _1
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _13)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _10)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _12)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _2)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _4)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _8)
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _15)
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _20)
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _21)
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _22)
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _23)
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _12)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _10)
  _28 <- iPre 1.0634800036678833 -< _27
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 2.391209595329409 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.8508305790742094 -< _0
  _2 <- pre1 1.079658531511982 -< _1
  _3 <- arr21 (+) -< {_0, _0}
  _4 <- arr21 (*) -< {_0, _2}
  _5 <- pre1 1.1143925971208914 -< _4
  _6 <- pre1 2.967639801827742 -< _5
  _7 <- arr21 (-) -< {_3, _1}
  _8 <- arr21 (+) -< {_6, _7}
  _9 <- arr21 (-) -< {_3, _8}
  _10 <- arr11 (+1) -< _2
  _11 <- arr11 (+1) -< _6
  _12 <- arr21 (-) -< {_5, _11}
  _13 <- arr21 (*) -< {_5, _0}
  _14 <- arr11 (+1) -< _1
  _15 <- arr21 (*) -< {_4, _13}
  _16 <- arr21 (+) -< {_9, _10}
  _17 <- arr21 (+) -< {_16, _12}
  _18 <- arr21 (+) -< {_6, _2}
  _19 <- arr21 (+) -< {_18, _4}
  _20 <- arr21 (+) -< {_17, _8}
  _21 <- arr21 (-) -< {_14, _15}
  _22 <- arr21 (*) -< {_9, _20}
  _23 <- arr21 (*) -< {_19, _21}
  _24 <- arr21 (-) -< {_13, _22}
  _25 <- arr21 (+) -< {_24, _23}
  _26 <- arr21 (*) -< {_25, _12}
  _27 <- arr21 (+) -< {_26, _10}
  _28 <- pre1 1.0634800036678833 -< _27
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 2.391209595329409 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0