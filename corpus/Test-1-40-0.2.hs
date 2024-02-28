{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- iPre 0.9443450576238994 -< _1
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _3)
    _6 <- iPre 2.5988064391314594 -< _1
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _6)
    _9 <- iPre 0.953190332336302 -< _5
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _3)
    _3 <- iPre 0.32484217065630994 -< _4
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _4)
  _12 <- FRP.Yampa.arr (+1) -< _11
  _13 <- iPre 1.1176989294702737 -< _7
  _14 <- FRP.Yampa.arr (+1) -< _10
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _13)
  _16 <- FRP.Yampa.arr (+1) -< _9
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _12)
  _18 <- iPre 1.5930251725041884 -< _14
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _10)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _19)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _12)
  _22 <- iPre 2.9481748315870475 -< _8
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _17)
  _24 <- FRP.Yampa.arr (+1) -< _23
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _3)
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _11)
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _23)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _26)
  _29 <- FRP.Yampa.arr (+1) -< _22
  _30 <- FRP.Yampa.arr (+1) -< _20
  _31 <- FRP.Yampa.arr (+1) -< _29
  _32 <- FRP.Yampa.arr (+1) -< _28
  _33 <- FRP.Yampa.arr (+1) -< _25
  _34 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _31)
  _35 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _34)
  _36 <- FRP.Yampa.arr (+1) -< _35
  _37 <- FRP.Yampa.arr (uncurry (*)) -< (_32, _33)
  _38 <- FRP.Yampa.arr (+1) -< _37
  _39 <- FRP.Yampa.arr (uncurry (-)) -< (_38, _30)
  _40 <- FRP.Yampa.arr (uncurry (+)) -< (_36, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- pre1 0.9443450576238994 -< _1
  rec
    _4 <- arr21 (-) -< {_2, _2}
    _5 <- arr21 (*) -< {_4, _3}
    _6 <- pre1 2.5988064391314594 -< _1
    _7 <- arr21 (+) -< {_0, _2}
    _8 <- arr21 (+) -< {_5, _6}
    _9 <- pre1 0.953190332336302 -< _5
    _10 <- arr21 (+) -< {_3, _3}
    _3 <- pre1 0.32484217065630994 -< _4

  _11 <- arr21 (+) -< {_5, _4}
  _12 <- arr11 (+1) -< _11
  _13 <- pre1 1.1176989294702737 -< _7
  _14 <- arr11 (+1) -< _10
  _15 <- arr21 (*) -< {_7, _13}
  _16 <- arr11 (+1) -< _9
  _17 <- arr21 (+) -< {_8, _12}
  _18 <- pre1 1.5930251725041884 -< _14
  _19 <- arr21 (*) -< {_0, _10}
  _20 <- arr21 (-) -< {_16, _19}
  _21 <- arr21 (+) -< {_15, _12}
  _22 <- pre1 2.9481748315870475 -< _8
  _23 <- arr21 (+) -< {_11, _17}
  _24 <- arr11 (+1) -< _23
  _25 <- arr21 (-) -< {_12, _3}
  _26 <- arr21 (-) -< {_7, _11}
  _27 <- arr21 (*) -< {_18, _23}
  _28 <- arr21 (*) -< {_27, _26}
  _29 <- arr11 (+1) -< _22
  _30 <- arr11 (+1) -< _20
  _31 <- arr11 (+1) -< _29
  _32 <- arr11 (+1) -< _28
  _33 <- arr11 (+1) -< _25
  _34 <- arr21 (-) -< {_21, _31}
  _35 <- arr21 (*) -< {_24, _34}
  _36 <- arr11 (+1) -< _35
  _37 <- arr21 (*) -< {_32, _33}
  _38 <- arr11 (+1) -< _37
  _39 <- arr21 (-) -< {_38, _30}
  _40 <- arr21 (+) -< {_36, _39}
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8