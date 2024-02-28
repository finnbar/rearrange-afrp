{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (+1) -< _1
    _3 <- FRP.Yampa.arr (+1) -< _1
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _5 <- FRP.Yampa.arr (+1) -< _1
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _7 <- iPre 2.159049084595051 -< _3
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _4)
    _9 <- iPre 0.2266732352901121 -< _6
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _2)
    _1 <- iPre 2.5212591984328188 -< _7
  _11 <- iPre 2.542135585841468 -< _8
  _12 <- iPre 1.2842871852862696 -< _10
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _9)
  _14 <- FRP.Yampa.arr (+1) -< _11
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _13)
  _16 <- iPre 2.073659944446194 -< _15
  _17 <- iPre 1.6076356726934808 -< _16
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- FRP.Yampa.arr (+1) -< _18
  _20 <- iPre 2.996289074067894 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _1
    _3 <- arr11 (+1) -< _1
    _4 <- arr21 (*) -< {_0, _0}
    _5 <- arr11 (+1) -< _1
    _6 <- arr21 (+) -< {_0, _1}
    _7 <- pre1 2.159049084595051 -< _3
    _8 <- arr21 (+) -< {_0, _4}
    _9 <- pre1 0.2266732352901121 -< _6
    _10 <- arr21 (+) -< {_5, _2}
    _1 <- pre1 2.5212591984328188 -< _7

  _11 <- pre1 2.542135585841468 -< _8
  _12 <- pre1 1.2842871852862696 -< _10
  _13 <- arr21 (-) -< {_12, _9}
  _14 <- arr11 (+1) -< _11
  _15 <- arr21 (+) -< {_14, _13}
  _16 <- pre1 2.073659944446194 -< _15
  _17 <- pre1 1.6076356726934808 -< _16
  _18 <- arr11 (+1) -< _17
  _19 <- arr11 (+1) -< _18
  _20 <- pre1 2.996289074067894 -< _19
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10