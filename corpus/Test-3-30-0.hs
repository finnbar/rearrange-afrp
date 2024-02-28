{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- iPre 2.7675010538870906 -< _0
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _2)
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _5)
  _7 <- iPre 2.123761960471804 -< _0
  _8 <- iPre 1.1975202102970663 -< _4
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _7)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _9)
  _11 <- iPre 4.505602510096392e-3 -< _7
  _12 <- iPre 1.926427868534644 -< _2
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _11)
  _14 <- iPre 1.8917932978100649 -< _6
  _15 <- iPre 2.0140011527268395 -< _14
  _16 <- iPre 0.687605912929037 -< _10
  _17 <- FRP.Yampa.arr (+1) -< _15
  _18 <- FRP.Yampa.arr (+1) -< _12
  _19 <- iPre 0.6417195013266179 -< _16
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _2)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _11)
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _21)
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _13)
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _22)
  _25 <- FRP.Yampa.arr (+1) -< _23
  _26 <- iPre 2.472982432250621 -< _24
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _19)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _25)
  _29 <- iPre 2.285190123999238 -< _28
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- pre1 2.7675010538870906 -< _0
  _3 <- arr21 (*) -< {_1, _0}
  _4 <- arr21 (+) -< {_3, _2}
  _5 <- arr21 (*) -< {_0, _1}
  _6 <- arr21 (*) -< {_4, _5}
  _7 <- pre1 2.123761960471804 -< _0
  _8 <- pre1 1.1975202102970663 -< _4
  _9 <- arr21 (*) -< {_6, _7}
  _10 <- arr21 (*) -< {_8, _9}
  _11 <- pre1 4.505602510096392e-3 -< _7
  _12 <- pre1 1.926427868534644 -< _2
  _13 <- arr21 (*) -< {_12, _11}
  _14 <- pre1 1.8917932978100649 -< _6
  _15 <- pre1 2.0140011527268395 -< _14
  _16 <- pre1 0.687605912929037 -< _10
  _17 <- arr11 (+1) -< _15
  _18 <- arr11 (+1) -< _12
  _19 <- pre1 0.6417195013266179 -< _16
  _20 <- arr21 (+) -< {_9, _2}
  _21 <- arr21 (+) -< {_20, _11}
  _22 <- arr21 (+) -< {_6, _21}
  _23 <- arr21 (-) -< {_18, _13}
  _24 <- arr21 (-) -< {_17, _22}
  _25 <- arr11 (+1) -< _23
  _26 <- pre1 2.472982432250621 -< _24
  _27 <- arr21 (+) -< {_26, _19}
  _28 <- arr21 (*) -< {_27, _25}
  _29 <- pre1 2.285190123999238 -< _28
  _30 <- arr11 (+1) -< _29
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0