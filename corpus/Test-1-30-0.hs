{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.8975096202546345 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _1)
  _5 <- iPre 1.6629729177629204 -< _0
  _6 <- FRP.Yampa.arr (+1) -< _3
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _1)
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _0)
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _8)
  _10 <- FRP.Yampa.arr (+1) -< _6
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
  _12 <- iPre 0.8730755854827331 -< _11
  _13 <- iPre 1.9739810418874868 -< _1
  _14 <- FRP.Yampa.arr (+1) -< _8
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _12)
  _16 <- FRP.Yampa.arr (+1) -< _11
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _15)
  _18 <- FRP.Yampa.arr (+1) -< _4
  _19 <- iPre 0.6540229236075248 -< _17
  _20 <- iPre 0.3615536559904861 -< _16
  _21 <- iPre 2.701114452033802 -< _18
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _10)
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _20)
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _19)
  _25 <- FRP.Yampa.arr (+1) -< _23
  _26 <- iPre 1.7635030341740978 -< _22
  _27 <- FRP.Yampa.arr (+1) -< _24
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _25)
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _28)
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.8975096202546345 -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- arr11 (+1) -< _2
  _4 <- arr21 (*) -< {_3, _1}
  _5 <- pre1 1.6629729177629204 -< _0
  _6 <- arr11 (+1) -< _3
  _7 <- arr21 (-) -< {_5, _1}
  _8 <- arr21 (-) -< {_7, _0}
  _9 <- arr21 (-) -< {_0, _8}
  _10 <- arr11 (+1) -< _6
  _11 <- arr21 (-) -< {_0, _1}
  _12 <- pre1 0.8730755854827331 -< _11
  _13 <- pre1 1.9739810418874868 -< _1
  _14 <- arr11 (+1) -< _8
  _15 <- arr21 (*) -< {_1, _12}
  _16 <- arr11 (+1) -< _11
  _17 <- arr21 (+) -< {_13, _15}
  _18 <- arr11 (+1) -< _4
  _19 <- pre1 0.6540229236075248 -< _17
  _20 <- pre1 0.3615536559904861 -< _16
  _21 <- pre1 2.701114452033802 -< _18
  _22 <- arr21 (+) -< {_9, _10}
  _23 <- arr21 (*) -< {_14, _20}
  _24 <- arr21 (*) -< {_21, _19}
  _25 <- arr11 (+1) -< _23
  _26 <- pre1 1.7635030341740978 -< _22
  _27 <- arr11 (+1) -< _24
  _28 <- arr21 (-) -< {_26, _25}
  _29 <- arr21 (*) -< {_27, _28}
  _30 <- arr11 (+1) -< _29
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0