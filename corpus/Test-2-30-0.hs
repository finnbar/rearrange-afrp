{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- iPre 2.3122839166210505 -< _2
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
  _5 <- FRP.Yampa.arr (+1) -< _3
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _7 <- FRP.Yampa.arr (+1) -< _5
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _2)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _8)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _8)
  _11 <- iPre 2.458179319327088 -< _9
  _12 <- iPre 1.651033418575032 -< _3
  _13 <- FRP.Yampa.arr (+1) -< _7
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _5)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _12)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _3)
  _17 <- iPre 1.5052201632607554 -< _16
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _12)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _10)
  _20 <- iPre 0.8952619542431987 -< _17
  _21 <- FRP.Yampa.arr (+1) -< _4
  _22 <- FRP.Yampa.arr (+1) -< _13
  _23 <- FRP.Yampa.arr (+1) -< _21
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _20)
  _25 <- FRP.Yampa.arr (+1) -< _15
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _23)
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _18)
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _26)
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _19)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _27)
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr11 (+1) -< _1
  _3 <- pre1 2.3122839166210505 -< _2
  _4 <- arr21 (-) -< {_2, _2}
  _5 <- arr11 (+1) -< _3
  _6 <- arr21 (-) -< {_0, _0}
  _7 <- arr11 (+1) -< _5
  _8 <- arr21 (*) -< {_6, _2}
  _9 <- arr21 (*) -< {_5, _8}
  _10 <- arr21 (+) -< {_8, _8}
  _11 <- pre1 2.458179319327088 -< _9
  _12 <- pre1 1.651033418575032 -< _3
  _13 <- arr11 (+1) -< _7
  _14 <- arr21 (+) -< {_3, _5}
  _15 <- arr21 (+) -< {_1, _12}
  _16 <- arr21 (*) -< {_1, _3}
  _17 <- pre1 1.5052201632607554 -< _16
  _18 <- arr21 (-) -< {_11, _12}
  _19 <- arr21 (-) -< {_16, _10}
  _20 <- pre1 0.8952619542431987 -< _17
  _21 <- arr11 (+1) -< _4
  _22 <- arr11 (+1) -< _13
  _23 <- arr11 (+1) -< _21
  _24 <- arr21 (+) -< {_14, _20}
  _25 <- arr11 (+1) -< _15
  _26 <- arr21 (*) -< {_25, _23}
  _27 <- arr21 (*) -< {_22, _18}
  _28 <- arr21 (-) -< {_24, _26}
  _29 <- arr21 (*) -< {_28, _19}
  _30 <- arr21 (-) -< {_29, _27}
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0