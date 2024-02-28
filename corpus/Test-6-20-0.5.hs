{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- iPre 0.5876078378382109 -< _1
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
  rec
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _6 <- FRP.Yampa.arr (+1) -< _4
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _5)
    _8 <- iPre 0.8930734216445887 -< _7
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _8)
    _10 <- iPre 2.0526817122064447 -< _3
    _11 <- FRP.Yampa.arr (+1) -< _8
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _10)
    _13 <- FRP.Yampa.arr (+1) -< _9
    _4 <- iPre 2.032583756391153 -< _11
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _1)
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _12)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _15)
  _17 <- iPre 1.1716695109924122 -< _13
  _18 <- FRP.Yampa.arr (+1) -< _16
  _19 <- FRP.Yampa.arr (+1) -< _17
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- pre1 0.5876078378382109 -< _1
  _3 <- arr21 (-) -< {_2, _1}
  rec
    _5 <- arr21 (+) -< {_0, _0}
    _6 <- arr11 (+1) -< _4
    _7 <- arr21 (-) -< {_2, _5}
    _8 <- pre1 0.8930734216445887 -< _7
    _9 <- arr21 (*) -< {_5, _8}
    _10 <- pre1 2.0526817122064447 -< _3
    _11 <- arr11 (+1) -< _8
    _12 <- arr21 (+) -< {_6, _10}
    _13 <- arr11 (+1) -< _9
    _4 <- pre1 2.032583756391153 -< _11

  _14 <- arr21 (-) -< {_4, _1}
  _15 <- arr21 (-) -< {_0, _12}
  _16 <- arr21 (+) -< {_14, _15}
  _17 <- pre1 1.1716695109924122 -< _13
  _18 <- arr11 (+1) -< _16
  _19 <- arr11 (+1) -< _17
  _20 <- arr21 (*) -< {_18, _19}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10