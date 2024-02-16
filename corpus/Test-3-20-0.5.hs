{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  rec
    _3 <- FRP.Yampa.arr (+1) -< _1
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- iPre 1.7297149817044735 -< _1
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
    _7 <- FRP.Yampa.arr (+1) -< _2
    _8 <- FRP.Yampa.arr (+1) -< _1
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _8)
    _10 <- FRP.Yampa.arr (+1) -< _9
    _11 <- FRP.Yampa.arr (+1) -< _0
    _2 <- iPre 1.9392916486847704 -< _10
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _5)
  _13 <- iPre 1.7628162178241311 -< _3
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _12)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _6)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _15)
  _17 <- iPre 1.9129128082317564 -< _16
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _13)
  _19 <- FRP.Yampa.arr (+1) -< _18
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _17)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  rec
    _3 <- arr11 (+1) -< _1
    _4 <- arr11 (+1) -< _1
    _5 <- pre1 1.7297149817044735 -< _1
    _6 <- arr21 (-) -< {_0, _1}
    _7 <- arr11 (+1) -< _2
    _8 <- arr11 (+1) -< _1
    _9 <- arr21 (-) -< {_0, _8}
    _10 <- arr11 (+1) -< _9
    _11 <- arr11 (+1) -< _0
    _2 <- pre1 1.9392916486847704 -< _10

  _12 <- arr21 (*) -< {_7, _5}
  _13 <- pre1 1.7628162178241311 -< _3
  _14 <- arr21 (*) -< {_1, _12}
  _15 <- arr21 (+) -< {_14, _6}
  _16 <- arr21 (+) -< {_4, _15}
  _17 <- pre1 1.9129128082317564 -< _16
  _18 <- arr21 (-) -< {_11, _13}
  _19 <- arr11 (+1) -< _18
  _20 <- arr21 (*) -< {_19, _17}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10