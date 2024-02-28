{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- iPre 0.3681530282493955 -< _0
  rec
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _0)
    _5 <- iPre 0.8503594319476251 -< _2
    _6 <- iPre 0.9540028698412033 -< _0
    _7 <- FRP.Yampa.arr (+1) -< _4
    _8 <- iPre 0.4844091163239939 -< _2
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- FRP.Yampa.arr (+1) -< _3
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _3)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _6)
    _3 <- iPre 2.0897191714750614 -< _9
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _5)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _8)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _13)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _1)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _16)
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- iPre 1.5447339652830254 -< _18
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _14)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- pre1 0.3681530282493955 -< _0
  rec
    _4 <- arr21 (+) -< {_3, _0}
    _5 <- pre1 0.8503594319476251 -< _2
    _6 <- pre1 0.9540028698412033 -< _0
    _7 <- arr11 (+1) -< _4
    _8 <- pre1 0.4844091163239939 -< _2
    _9 <- arr11 (+1) -< _6
    _10 <- arr11 (+1) -< _3
    _11 <- arr21 (*) -< {_2, _3}
    _12 <- arr21 (*) -< {_10, _6}
    _3 <- pre1 2.0897191714750614 -< _9

  _13 <- arr21 (+) -< {_6, _5}
  _14 <- arr21 (+) -< {_12, _8}
  _15 <- arr21 (+) -< {_11, _13}
  _16 <- arr21 (+) -< {_15, _1}
  _17 <- arr21 (+) -< {_7, _16}
  _18 <- arr11 (+1) -< _17
  _19 <- pre1 1.5447339652830254 -< _18
  _20 <- arr21 (+) -< {_19, _14}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10