{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- iPre 1.760455834429636 -< _0
  rec
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _1)
    _6 <- iPre 0.39030252737643606 -< _4
    _7 <- FRP.Yampa.arr (+1) -< _2
    _8 <- iPre 1.5018582665421527 -< _1
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _2)
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _9)
    _11 <- iPre 2.8588096823637548 -< _2
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _5)
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _11)
    _14 <- FRP.Yampa.arr (+1) -< _4
    _15 <- FRP.Yampa.arr (+1) -< _0
    _16 <- iPre 0.17495644393027684 -< _7
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _10)
    _18 <- FRP.Yampa.arr (+1) -< _17
    _19 <- iPre 0.7646564795301198 -< _14
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _14)
    _21 <- iPre 0.298847290607615 -< _2
    _22 <- FRP.Yampa.arr (+1) -< _11
    _23 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _13)
    _4 <- iPre 1.1023184584211536 -< _8
  _24 <- FRP.Yampa.arr (+1) -< _18
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _24)
  _26 <- FRP.Yampa.arr (+1) -< _25
  _27 <- FRP.Yampa.arr (+1) -< _2
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _16)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _20)
  _31 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _21)
  _32 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _22)
  _33 <- FRP.Yampa.arr (uncurry (*)) -< (_31, _32)
  _34 <- FRP.Yampa.arr (+1) -< _3
  _35 <- FRP.Yampa.arr (uncurry (-)) -< (_33, _29)
  _36 <- FRP.Yampa.arr (+1) -< _34
  _37 <- FRP.Yampa.arr (+1) -< _35
  _38 <- iPre 0.9476759592169574 -< _37
  _39 <- FRP.Yampa.arr (uncurry (-)) -< (_36, _38)
  _40 <- iPre 1.0928936615923153 -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  _3 <- pre1 1.760455834429636 -< _0
  rec
    _5 <- arr21 (*) -< {_4, _1}
    _6 <- pre1 0.39030252737643606 -< _4
    _7 <- arr11 (+1) -< _2
    _8 <- pre1 1.5018582665421527 -< _1
    _9 <- arr21 (+) -< {_6, _2}
    _10 <- arr21 (-) -< {_6, _9}
    _11 <- pre1 2.8588096823637548 -< _2
    _12 <- arr21 (+) -< {_9, _5}
    _13 <- arr21 (-) -< {_2, _11}
    _14 <- arr11 (+1) -< _4
    _15 <- arr11 (+1) -< _0
    _16 <- pre1 0.17495644393027684 -< _7
    _17 <- arr21 (+) -< {_6, _10}
    _18 <- arr11 (+1) -< _17
    _19 <- pre1 0.7646564795301198 -< _14
    _20 <- arr21 (-) -< {_19, _14}
    _21 <- pre1 0.298847290607615 -< _2
    _22 <- arr11 (+1) -< _11
    _23 <- arr21 (+) -< {_15, _13}
    _4 <- pre1 1.1023184584211536 -< _8

  _24 <- arr11 (+1) -< _18
  _25 <- arr21 (-) -< {_23, _24}
  _26 <- arr11 (+1) -< _25
  _27 <- arr11 (+1) -< _2
  _28 <- arr11 (+1) -< _27
  _29 <- arr21 (+) -< {_12, _16}
  _30 <- arr21 (*) -< {_28, _20}
  _31 <- arr21 (*) -< {_26, _21}
  _32 <- arr21 (+) -< {_30, _22}
  _33 <- arr21 (*) -< {_31, _32}
  _34 <- arr11 (+1) -< _3
  _35 <- arr21 (-) -< {_33, _29}
  _36 <- arr11 (+1) -< _34
  _37 <- arr11 (+1) -< _35
  _38 <- pre1 0.9476759592169574 -< _37
  _39 <- arr21 (-) -< {_36, _38}
  _40 <- pre1 1.0928936615923153 -< _39
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20