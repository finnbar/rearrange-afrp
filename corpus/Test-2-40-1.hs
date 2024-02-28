{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (+1) -< _0
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _0)
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
    _6 <- iPre 2.4065555706440316 -< _5
    _7 <- FRP.Yampa.arr (+1) -< _2
    _8 <- iPre 1.9213199662417888 -< _3
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _6)
    _10 <- FRP.Yampa.arr (+1) -< _0
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _10)
    _12 <- FRP.Yampa.arr (+1) -< _8
    _13 <- FRP.Yampa.arr (+1) -< _0
    _14 <- iPre 0.6245541274156886 -< _12
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _6)
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _5)
    _17 <- FRP.Yampa.arr (+1) -< _4
    _18 <- FRP.Yampa.arr (+1) -< _6
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _17)
    _20 <- FRP.Yampa.arr (+1) -< _7
    _21 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _20)
    _22 <- FRP.Yampa.arr (+1) -< _11
    _23 <- FRP.Yampa.arr (+1) -< _19
    _24 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _0)
    _25 <- FRP.Yampa.arr (+1) -< _15
    _26 <- FRP.Yampa.arr (+1) -< _9
    _27 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _25)
    _28 <- iPre 9.520737075165857e-2 -< _26
    _29 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _18)
    _30 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _22)
    _31 <- FRP.Yampa.arr (+1) -< _30
    _32 <- iPre 0.7935693734404913 -< _29
    _33 <- FRP.Yampa.arr (+1) -< _32
    _34 <- FRP.Yampa.arr (+1) -< _24
    _35 <- FRP.Yampa.arr (+1) -< _34
    _36 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _31)
    _37 <- iPre 1.4973923545143777 -< _36
    _38 <- FRP.Yampa.arr (uncurry (+)) -< (_33, _35)
    _39 <- FRP.Yampa.arr (+1) -< _37
    _40 <- iPre 1.3873116784074289 -< _39
    _1 <- iPre 0.9755774632172927 -< _38
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _0
    _3 <- arr21 (*) -< {_2, _0}
    _4 <- arr21 (+) -< {_0, _1}
    _5 <- arr21 (-) -< {_2, _1}
    _6 <- pre1 2.4065555706440316 -< _5
    _7 <- arr11 (+1) -< _2
    _8 <- pre1 1.9213199662417888 -< _3
    _9 <- arr21 (*) -< {_4, _6}
    _10 <- arr11 (+1) -< _0
    _11 <- arr21 (-) -< {_4, _10}
    _12 <- arr11 (+1) -< _8
    _13 <- arr11 (+1) -< _0
    _14 <- pre1 0.6245541274156886 -< _12
    _15 <- arr21 (*) -< {_8, _6}
    _16 <- arr21 (+) -< {_15, _5}
    _17 <- arr11 (+1) -< _4
    _18 <- arr11 (+1) -< _6
    _19 <- arr21 (+) -< {_16, _17}
    _20 <- arr11 (+1) -< _7
    _21 <- arr21 (-) -< {_14, _20}
    _22 <- arr11 (+1) -< _11
    _23 <- arr11 (+1) -< _19
    _24 <- arr21 (+) -< {_21, _0}
    _25 <- arr11 (+1) -< _15
    _26 <- arr11 (+1) -< _9
    _27 <- arr21 (*) -< {_13, _25}
    _28 <- pre1 9.520737075165857e-2 -< _26
    _29 <- arr21 (+) -< {_28, _18}
    _30 <- arr21 (-) -< {_23, _22}
    _31 <- arr11 (+1) -< _30
    _32 <- pre1 0.7935693734404913 -< _29
    _33 <- arr11 (+1) -< _32
    _34 <- arr11 (+1) -< _24
    _35 <- arr11 (+1) -< _34
    _36 <- arr21 (*) -< {_27, _31}
    _37 <- pre1 1.4973923545143777 -< _36
    _38 <- arr21 (+) -< {_33, _35}
    _39 <- arr11 (+1) -< _37
    _40 <- pre1 1.3873116784074289 -< _39
    _1 <- pre1 0.9755774632172927 -< _38

  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40