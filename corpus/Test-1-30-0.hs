{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.9898363458303185 -< _0
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _3)
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _5)
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _7)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _2)
  _11 <- iPre 8.479096468780381e-2 -< _2
  _12 <- FRP.Yampa.arr (+1) -< _9
  _13 <- FRP.Yampa.arr (+1) -< _0
  _14 <- iPre 2.8600612165001746 -< _8
  _15 <- FRP.Yampa.arr (+1) -< _14
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _12)
  _17 <- iPre 0.7220581060761687 -< _15
  _18 <- iPre 2.7568452764336873 -< _12
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _7)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _10)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _19)
  _22 <- iPre 1.0587484115880526 -< _21
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _16)
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _13)
  _25 <- FRP.Yampa.arr (+1) -< _18
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _25)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _24)
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- iPre 0.9334926850262363 -< _28
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.9898363458303185 -< _0
  _2 <- arr21 (+) -< {_0, _1}
  _3 <- arr21 (*) -< {_2, _1}
  _4 <- arr21 (-) -< {_2, _3}
  _5 <- arr21 (+) -< {_4, _3}
  _6 <- arr21 (+) -< {_2, _0}
  _7 <- arr11 (+1) -< _6
  _8 <- arr21 (-) -< {_7, _5}
  _9 <- arr21 (-) -< {_5, _7}
  _10 <- arr21 (+) -< {_3, _2}
  _11 <- pre1 8.479096468780381e-2 -< _2
  _12 <- arr11 (+1) -< _9
  _13 <- arr11 (+1) -< _0
  _14 <- pre1 2.8600612165001746 -< _8
  _15 <- arr11 (+1) -< _14
  _16 <- arr21 (+) -< {_7, _12}
  _17 <- pre1 0.7220581060761687 -< _15
  _18 <- pre1 2.7568452764336873 -< _12
  _19 <- arr21 (-) -< {_9, _7}
  _20 <- arr21 (-) -< {_11, _10}
  _21 <- arr21 (+) -< {_20, _19}
  _22 <- pre1 1.0587484115880526 -< _21
  _23 <- arr21 (+) -< {_22, _16}
  _24 <- arr21 (+) -< {_17, _13}
  _25 <- arr11 (+1) -< _18
  _26 <- arr21 (-) -< {_23, _25}
  _27 <- arr21 (+) -< {_26, _24}
  _28 <- arr11 (+1) -< _27
  _29 <- pre1 0.9334926850262363 -< _28
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0