{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _3)
    _6 <- FRP.Yampa.arr (+1) -< _4
    _1 <- iPre 2.4209477148001666 -< _1
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (+1) -< _3
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _8)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _9)
  _11 <- FRP.Yampa.arr (+1) -< _3
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _8)
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _0)
  _14 <- FRP.Yampa.arr (+1) -< _1
  _15 <- iPre 1.8245619069417809 -< _7
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _5)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _9)
  _18 <- iPre 0.6031933674364597 -< _11
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _15)
  _20 <- FRP.Yampa.arr (+1) -< _13
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _19)
  _22 <- iPre 1.214021284028879 -< _18
  _23 <- FRP.Yampa.arr (+1) -< _17
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _20)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _21)
  _26 <- iPre 2.886441757790378 -< _25
  _27 <- iPre 1.1766458658943895 -< _23
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _26)
  _29 <- iPre 0.39418519397538104 -< _22
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _28)
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_1, _0}
    _3 <- arr21 (-) -< {_1, _2}
    _4 <- arr21 (-) -< {_1, _0}
    _5 <- arr21 (-) -< {_0, _3}
    _6 <- arr11 (+1) -< _4
    _1 <- pre1 2.4209477148001666 -< _1

  _7 <- arr11 (+1) -< _6
  _8 <- arr11 (+1) -< _3
  _9 <- arr21 (-) -< {_0, _8}
  _10 <- arr21 (+) -< {_0, _9}
  _11 <- arr11 (+1) -< _3
  _12 <- arr21 (*) -< {_3, _8}
  _13 <- arr21 (-) -< {_9, _0}
  _14 <- arr11 (+1) -< _1
  _15 <- pre1 1.8245619069417809 -< _7
  _16 <- arr21 (-) -< {_12, _5}
  _17 <- arr21 (-) -< {_7, _9}
  _18 <- pre1 0.6031933674364597 -< _11
  _19 <- arr21 (+) -< {_16, _15}
  _20 <- arr11 (+1) -< _13
  _21 <- arr21 (*) -< {_10, _19}
  _22 <- pre1 1.214021284028879 -< _18
  _23 <- arr11 (+1) -< _17
  _24 <- arr21 (*) -< {_14, _20}
  _25 <- arr21 (-) -< {_24, _21}
  _26 <- pre1 2.886441757790378 -< _25
  _27 <- pre1 1.1766458658943895 -< _23
  _28 <- arr21 (+) -< {_27, _26}
  _29 <- pre1 0.39418519397538104 -< _22
  _30 <- arr21 (-) -< {_29, _28}
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6