{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.19675300548654648 -< _0
  rec
    _3 <- iPre 2.956925986969861 -< _0
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- FRP.Yampa.arr (+1) -< _1
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _0)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _2)
    _11 <- iPre 0.9262983441236996 -< _8
    _2 <- iPre 0.3367110539408511 -< _9
  _12 <- iPre 1.2709586972045892 -< _10
  _13 <- FRP.Yampa.arr (+1) -< _4
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _7)
  _15 <- FRP.Yampa.arr (+1) -< _13
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- FRP.Yampa.arr (+1) -< _15
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _17)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _3)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.19675300548654648 -< _0
  rec
    _3 <- pre1 2.956925986969861 -< _0
    _4 <- arr11 (+1) -< _1
    _5 <- arr11 (+1) -< _1
    _6 <- arr21 (-) -< {_1, _0}
    _7 <- arr21 (-) -< {_0, _1}
    _8 <- arr21 (*) -< {_0, _1}
    _9 <- arr21 (-) -< {_5, _0}
    _10 <- arr21 (*) -< {_6, _2}
    _11 <- pre1 0.9262983441236996 -< _8
    _2 <- pre1 0.3367110539408511 -< _9

  _12 <- pre1 1.2709586972045892 -< _10
  _13 <- arr11 (+1) -< _4
  _14 <- arr21 (+) -< {_12, _7}
  _15 <- arr11 (+1) -< _13
  _16 <- arr11 (+1) -< _14
  _17 <- arr11 (+1) -< _15
  _18 <- arr21 (+) -< {_11, _17}
  _19 <- arr21 (-) -< {_18, _3}
  _20 <- arr21 (*) -< {_16, _19}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10