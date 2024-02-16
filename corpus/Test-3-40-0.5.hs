{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- iPre 2.2534546080410363 -< _1
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _2)
    _6 <- iPre 3.0400008690404903 -< _3
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _6)
    _8 <- iPre 1.2490972200084651 -< _7
    _9 <- iPre 0.16664403111656437 -< _8
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _9)
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _2)
    _12 <- FRP.Yampa.arr (+1) -< _0
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _12)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _11)
    _15 <- FRP.Yampa.arr (+1) -< _12
    _16 <- iPre 0.6209966464079497 -< _15
    _17 <- iPre 0.5710219150377858 -< _3
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _1)
    _19 <- iPre 1.389325250967628 -< _14
    _20 <- FRP.Yampa.arr (+1) -< _17
    _21 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _19)
    _22 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _21)
    _3 <- iPre 1.4830792662296508 -< _10
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _20)
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _10)
  _25 <- FRP.Yampa.arr (+1) -< _10
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _8)
  _27 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _24)
  _28 <- FRP.Yampa.arr (+1) -< _22
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _28)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _26)
  _31 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _5)
  _32 <- FRP.Yampa.arr (+1) -< _31
  _33 <- FRP.Yampa.arr (uncurry (-)) -< (_32, _29)
  _34 <- iPre 1.4753947243748202 -< _3
  _35 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _33)
  _36 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _35)
  _37 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _34)
  _38 <- iPre 2.2441882010813328 -< _37
  _39 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _38)
  _40 <- FRP.Yampa.arr (uncurry (+)) -< (_36, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- pre1 2.2534546080410363 -< _1
  rec
    _4 <- arr21 (-) -< {_1, _3}
    _5 <- arr21 (*) -< {_4, _2}
    _6 <- pre1 3.0400008690404903 -< _3
    _7 <- arr21 (*) -< {_5, _6}
    _8 <- pre1 1.2490972200084651 -< _7
    _9 <- pre1 0.16664403111656437 -< _8
    _10 <- arr21 (*) -< {_4, _9}
    _11 <- arr21 (-) -< {_10, _2}
    _12 <- arr11 (+1) -< _0
    _13 <- arr21 (*) -< {_6, _12}
    _14 <- arr21 (+) -< {_5, _11}
    _15 <- arr11 (+1) -< _12
    _16 <- pre1 0.6209966464079497 -< _15
    _17 <- pre1 0.5710219150377858 -< _3
    _18 <- arr21 (+) -< {_16, _1}
    _19 <- pre1 1.389325250967628 -< _14
    _20 <- arr11 (+1) -< _17
    _21 <- arr21 (+) -< {_13, _19}
    _22 <- arr21 (-) -< {_9, _21}
    _3 <- pre1 1.4830792662296508 -< _10

  _23 <- arr21 (-) -< {_0, _20}
  _24 <- arr21 (*) -< {_20, _10}
  _25 <- arr11 (+1) -< _10
  _26 <- arr21 (*) -< {_18, _8}
  _27 <- arr21 (-) -< {_23, _24}
  _28 <- arr11 (+1) -< _22
  _29 <- arr21 (+) -< {_25, _28}
  _30 <- arr21 (*) -< {_27, _26}
  _31 <- arr21 (-) -< {_9, _5}
  _32 <- arr11 (+1) -< _31
  _33 <- arr21 (-) -< {_32, _29}
  _34 <- pre1 1.4753947243748202 -< _3
  _35 <- arr21 (*) -< {_27, _33}
  _36 <- arr21 (+) -< {_14, _35}
  _37 <- arr21 (-) -< {_24, _34}
  _38 <- pre1 2.2441882010813328 -< _37
  _39 <- arr21 (+) -< {_30, _38}
  _40 <- arr21 (+) -< {_36, _39}
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20