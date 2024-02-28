{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.2425820613134258 -< _0
  _2 <- iPre 0.43739900821632116 -< _0
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _3)
  rec
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
    _7 <- iPre 1.3707199439389572 -< _0
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _4)
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _0)
    _10 <- iPre 0.1298164310000547 -< _1
    _5 <- iPre 2.281769829362355 -< _3
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _3)
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _6)
  _13 <- FRP.Yampa.arr (+1) -< _12
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _13)
  _15 <- FRP.Yampa.arr (+1) -< _14
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _11)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _9)
  _18 <- iPre 3.4343151924403026 -< _4
  _19 <- FRP.Yampa.arr (+1) -< _16
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _19)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _0)
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _20)
  _23 <- FRP.Yampa.arr (+1) -< _22
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _23)
  _25 <- FRP.Yampa.arr (+1) -< _4
  _26 <- FRP.Yampa.arr (+1) -< _25
  _27 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _24)
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _27)
  _29 <- iPre 0.8136755802227092 -< _28
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _26)
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.2425820613134258 -< _0
  _2 <- pre1 0.43739900821632116 -< _0
  _3 <- arr21 (*) -< {_1, _1}
  _4 <- arr21 (+) -< {_3, _3}
  rec
    _6 <- arr21 (+) -< {_2, _2}
    _7 <- pre1 1.3707199439389572 -< _0
    _8 <- arr21 (-) -< {_5, _4}
    _9 <- arr21 (+) -< {_8, _0}
    _10 <- pre1 0.1298164310000547 -< _1
    _5 <- pre1 2.281769829362355 -< _3

  _11 <- arr21 (*) -< {_7, _3}
  _12 <- arr21 (+) -< {_10, _6}
  _13 <- arr11 (+1) -< _12
  _14 <- arr21 (-) -< {_11, _13}
  _15 <- arr11 (+1) -< _14
  _16 <- arr21 (*) -< {_2, _11}
  _17 <- arr21 (+) -< {_15, _9}
  _18 <- pre1 3.4343151924403026 -< _4
  _19 <- arr11 (+1) -< _16
  _20 <- arr21 (+) -< {_18, _19}
  _21 <- arr21 (+) -< {_17, _0}
  _22 <- arr21 (*) -< {_21, _20}
  _23 <- arr11 (+1) -< _22
  _24 <- arr21 (-) -< {_10, _23}
  _25 <- arr11 (+1) -< _4
  _26 <- arr11 (+1) -< _25
  _27 <- arr21 (-) -< {_13, _24}
  _28 <- arr21 (-) -< {_1, _27}
  _29 <- pre1 0.8136755802227092 -< _28
  _30 <- arr21 (*) -< {_29, _26}
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6