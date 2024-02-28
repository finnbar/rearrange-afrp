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
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _4 <- iPre 1.213087158485567 -< _1
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
    _6 <- iPre 2.963129748702933 -< _2
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
    _8 <- FRP.Yampa.arr (+1) -< _2
    _9 <- FRP.Yampa.arr (+1) -< _3
    _10 <- iPre 6.602554817912683e-2 -< _9
    _11 <- FRP.Yampa.arr (+1) -< _8
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _9)
    _13 <- FRP.Yampa.arr (+1) -< _8
    _14 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _8)
    _15 <- FRP.Yampa.arr (+1) -< _9
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _12)
    _17 <- iPre 0.8141763576458375 -< _9
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _2)
    _19 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _5)
    _20 <- FRP.Yampa.arr (+1) -< _19
    _21 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _17)
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _14)
    _23 <- iPre 0.3167754401262598 -< _7
    _24 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _20)
    _25 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _13)
    _26 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _22)
    _27 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _23)
    _28 <- iPre 2.918898146203292 -< _27
    _29 <- FRP.Yampa.arr (+1) -< _24
    _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _26)
    _1 <- iPre 2.1496091994982276 -< _28
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _0
    _3 <- arr21 (*) -< {_0, _0}
    _4 <- pre1 1.213087158485567 -< _1
    _5 <- arr21 (*) -< {_1, _2}
    _6 <- pre1 2.963129748702933 -< _2
    _7 <- arr21 (*) -< {_2, _1}
    _8 <- arr11 (+1) -< _2
    _9 <- arr11 (+1) -< _3
    _10 <- pre1 6.602554817912683e-2 -< _9
    _11 <- arr11 (+1) -< _8
    _12 <- arr21 (+) -< {_10, _9}
    _13 <- arr11 (+1) -< _8
    _14 <- arr21 (-) -< {_6, _8}
    _15 <- arr11 (+1) -< _9
    _16 <- arr21 (*) -< {_4, _12}
    _17 <- pre1 0.8141763576458375 -< _9
    _18 <- arr21 (*) -< {_4, _2}
    _19 <- arr21 (*) -< {_8, _5}
    _20 <- arr11 (+1) -< _19
    _21 <- arr21 (+) -< {_15, _17}
    _22 <- arr21 (+) -< {_18, _14}
    _23 <- pre1 0.3167754401262598 -< _7
    _24 <- arr21 (*) -< {_21, _20}
    _25 <- arr21 (+) -< {_11, _13}
    _26 <- arr21 (-) -< {_25, _22}
    _27 <- arr21 (*) -< {_16, _23}
    _28 <- pre1 2.918898146203292 -< _27
    _29 <- arr11 (+1) -< _24
    _30 <- arr21 (*) -< {_29, _26}
    _1 <- pre1 2.1496091994982276 -< _28

  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30