{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- iPre 2.523252406706632 -< _0
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _4 <- FRP.Yampa.arr (+1) -< _0
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _3)
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _5)
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _5)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _7)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _8)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _3)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _9)
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _6)
  _13 <- FRP.Yampa.arr (+1) -< _6
  _14 <- iPre 0.5144550890666637 -< _12
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _13)
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- iPre 0.3963962325789992 -< _15
  _18 <- iPre 0.32950899266858497 -< _3
  _19 <- FRP.Yampa.arr (+1) -< _2
  _20 <- FRP.Yampa.arr (+1) -< _16
  _21 <- FRP.Yampa.arr (+1) -< _18
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _17)
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _21)
  _24 <- iPre 1.9862980977631528 -< _11
  _25 <- FRP.Yampa.arr (+1) -< _23
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _20)
  _27 <- iPre 2.1216714050816647 -< _26
  _28 <- iPre 1.2933932174663383 -< _24
  _29 <- FRP.Yampa.arr (+1) -< _27
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- pre1 2.523252406706632 -< _0
  _3 <- arr21 (+) -< {_1, _0}
  _4 <- arr11 (+1) -< _0
  _5 <- arr21 (+) -< {_4, _3}
  _6 <- arr21 (+) -< {_2, _5}
  _7 <- arr21 (-) -< {_3, _5}
  _8 <- arr21 (*) -< {_3, _7}
  _9 <- arr21 (+) -< {_5, _8}
  _10 <- arr21 (*) -< {_3, _3}
  _11 <- arr21 (-) -< {_10, _9}
  _12 <- arr21 (-) -< {_7, _6}
  _13 <- arr11 (+1) -< _6
  _14 <- pre1 0.5144550890666637 -< _12
  _15 <- arr21 (+) -< {_4, _13}
  _16 <- arr11 (+1) -< _14
  _17 <- pre1 0.3963962325789992 -< _15
  _18 <- pre1 0.32950899266858497 -< _3
  _19 <- arr11 (+1) -< _2
  _20 <- arr11 (+1) -< _16
  _21 <- arr11 (+1) -< _18
  _22 <- arr21 (+) -< {_19, _17}
  _23 <- arr21 (+) -< {_22, _21}
  _24 <- pre1 1.9862980977631528 -< _11
  _25 <- arr11 (+1) -< _23
  _26 <- arr21 (+) -< {_25, _20}
  _27 <- pre1 2.1216714050816647 -< _26
  _28 <- pre1 1.2933932174663383 -< _24
  _29 <- arr11 (+1) -< _27
  _30 <- arr21 (+) -< {_28, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0