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
    _3 <- FRP.Yampa.arr (+1) -< _0
    _4 <- iPre 2.7127159496086617 -< _0
    _5 <- iPre 2.8916128655943716 -< _0
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _4)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _1)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
    _9 <- FRP.Yampa.arr (+1) -< _5
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _1)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _9)
    _12 <- iPre 2.2085194641165398 -< _2
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _2)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _7)
    _15 <- FRP.Yampa.arr (+1) -< _12
    _16 <- iPre 0.18969504440308801 -< _2
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _10)
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _16)
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _9)
    _20 <- FRP.Yampa.arr (+1) -< _9
    _21 <- iPre 2.3708048729275575 -< _5
    _22 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _3)
    _23 <- iPre 1.9545226190226437 -< _15
    _24 <- FRP.Yampa.arr (+1) -< _18
    _25 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _17)
    _26 <- FRP.Yampa.arr (+1) -< _19
    _27 <- iPre 1.2915773981327412 -< _20
    _28 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _14)
    _29 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _23)
    _30 <- iPre 0.2516714546984121 -< _27
    _31 <- FRP.Yampa.arr (+1) -< _29
    _32 <- iPre 0.32172582132557276 -< _11
    _33 <- iPre 1.403163302864232 -< _28
    _34 <- FRP.Yampa.arr (+1) -< _32
    _35 <- FRP.Yampa.arr (uncurry (+)) -< (_33, _34)
    _36 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _30)
    _37 <- iPre 0.12670456354216686 -< _36
    _38 <- FRP.Yampa.arr (uncurry (*)) -< (_35, _31)
    _39 <- FRP.Yampa.arr (uncurry (*)) -< (_37, _38)
    _40 <- iPre 0.6870897383234986 -< _8
    _1 <- iPre 0.6480990490582672 -< _40
  FRP.Yampa.returnA -< _39

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _0
    _3 <- arr11 (+1) -< _0
    _4 <- pre1 2.7127159496086617 -< _0
    _5 <- pre1 2.8916128655943716 -< _0
    _6 <- arr21 (*) -< {_2, _4}
    _7 <- arr21 (-) -< {_5, _1}
    _8 <- arr21 (*) -< {_0, _1}
    _9 <- arr11 (+1) -< _5
    _10 <- arr21 (-) -< {_6, _1}
    _11 <- arr21 (*) -< {_0, _9}
    _12 <- pre1 2.2085194641165398 -< _2
    _13 <- arr21 (-) -< {_7, _2}
    _14 <- arr21 (*) -< {_5, _7}
    _15 <- arr11 (+1) -< _12
    _16 <- pre1 0.18969504440308801 -< _2
    _17 <- arr21 (*) -< {_10, _10}
    _18 <- arr21 (*) -< {_7, _16}
    _19 <- arr21 (-) -< {_13, _9}
    _20 <- arr11 (+1) -< _9
    _21 <- pre1 2.3708048729275575 -< _5
    _22 <- arr21 (*) -< {_21, _3}
    _23 <- pre1 1.9545226190226437 -< _15
    _24 <- arr11 (+1) -< _18
    _25 <- arr21 (+) -< {_22, _17}
    _26 <- arr11 (+1) -< _19
    _27 <- pre1 1.2915773981327412 -< _20
    _28 <- arr21 (+) -< {_25, _14}
    _29 <- arr21 (+) -< {_24, _23}
    _30 <- pre1 0.2516714546984121 -< _27
    _31 <- arr11 (+1) -< _29
    _32 <- pre1 0.32172582132557276 -< _11
    _33 <- pre1 1.403163302864232 -< _28
    _34 <- arr11 (+1) -< _32
    _35 <- arr21 (+) -< {_33, _34}
    _36 <- arr21 (*) -< {_26, _30}
    _37 <- pre1 0.12670456354216686 -< _36
    _38 <- arr21 (*) -< {_35, _31}
    _39 <- arr21 (*) -< {_37, _38}
    _40 <- pre1 0.6870897383234986 -< _8
    _1 <- pre1 0.6480990490582672 -< _40

  AFRP.returnA -< _39|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40