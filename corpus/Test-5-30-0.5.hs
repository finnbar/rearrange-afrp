{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
    _5 <- FRP.Yampa.arr (+1) -< _4
    _6 <- FRP.Yampa.arr (+1) -< _5
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _3)
    _8 <- FRP.Yampa.arr (+1) -< _3
    _9 <- iPre 2.3675385163549523 -< _2
    _10 <- iPre 2.438551900390636 -< _6
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _7)
    _12 <- iPre 1.163318672696854 -< _10
    _13 <- iPre 2.9450068164310297 -< _8
    _14 <- iPre 4.909334698593241e-2 -< _1
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _8)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _12)
    _2 <- iPre 1.1875271142167314 -< _3
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _14)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _17)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _2)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _18)
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _15)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _18)
  _23 <- FRP.Yampa.arr (+1) -< _4
  _24 <- iPre 1.8768581739367884 -< _23
  _25 <- iPre 0.6779732044329124 -< _24
  _26 <- FRP.Yampa.arr (+1) -< _13
  _27 <- FRP.Yampa.arr (+1) -< _26
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _20)
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _27)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _25)
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  rec
    _3 <- arr21 (-) -< {_2, _1}
    _4 <- arr21 (*) -< {_3, _2}
    _5 <- arr11 (+1) -< _4
    _6 <- arr11 (+1) -< _5
    _7 <- arr21 (+) -< {_6, _3}
    _8 <- arr11 (+1) -< _3
    _9 <- pre1 2.3675385163549523 -< _2
    _10 <- pre1 2.438551900390636 -< _6
    _11 <- arr21 (*) -< {_2, _7}
    _12 <- pre1 1.163318672696854 -< _10
    _13 <- pre1 2.9450068164310297 -< _8
    _14 <- pre1 4.909334698593241e-2 -< _1
    _15 <- arr21 (*) -< {_11, _8}
    _16 <- arr21 (*) -< {_9, _12}
    _2 <- pre1 1.1875271142167314 -< _3

  _17 <- arr21 (+) -< {_5, _14}
  _18 <- arr21 (+) -< {_2, _17}
  _19 <- arr21 (-) -< {_15, _2}
  _20 <- arr21 (*) -< {_16, _18}
  _21 <- arr21 (*) -< {_19, _15}
  _22 <- arr21 (-) -< {_21, _18}
  _23 <- arr11 (+1) -< _4
  _24 <- pre1 1.8768581739367884 -< _23
  _25 <- pre1 0.6779732044329124 -< _24
  _26 <- arr11 (+1) -< _13
  _27 <- arr11 (+1) -< _26
  _28 <- arr21 (+) -< {_22, _20}
  _29 <- arr21 (*) -< {_28, _27}
  _30 <- arr21 (-) -< {_29, _25}
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15