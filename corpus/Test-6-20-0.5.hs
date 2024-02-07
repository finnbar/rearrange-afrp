{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- iPre 2.31895964957364 -< _0
  rec
    _5 <- FRP.Yampa.arr (+1) -< _4
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _4)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _6)
    _9 <- iPre 1.5491576245149048 -< _6
    _10 <- iPre 0.9738122628119196 -< _5
    _11 <- FRP.Yampa.arr (+1) -< _2
    _12 <- FRP.Yampa.arr (+1) -< _0
    _3 <- iPre 1.9947357169187119 -< _6
    _4 <- iPre 0.8620755265900784 -< _8
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _7)
  _14 <- FRP.Yampa.arr (+1) -< _11
  _15 <- iPre 5.028007114508385e-2 -< _13
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _10)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _15)
  _18 <- FRP.Yampa.arr (+1) -< _9
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _17)
  _20 <- iPre 0.7233698524596788 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- pre1 2.31895964957364 -< _0
  rec
    _5 <- arr11 (+1) -< _4
    _6 <- arr21 (-) -< {_0, _4}
    _7 <- arr21 (-) -< {_1, _3}
    _8 <- arr21 (+) -< {_0, _6}
    _9 <- pre1 1.5491576245149048 -< _6
    _10 <- pre1 0.9738122628119196 -< _5
    _11 <- arr11 (+1) -< _2
    _12 <- arr11 (+1) -< _0
    _3 <- pre1 1.9947357169187119 -< _6
    _4 <- pre1 0.8620755265900784 -< _8

  _13 <- arr21 (+) -< {_12, _7}
  _14 <- arr11 (+1) -< _11
  _15 <- pre1 5.028007114508385e-2 -< _13
  _16 <- arr21 (-) -< {_14, _10}
  _17 <- arr21 (+) -< {_16, _15}
  _18 <- arr11 (+1) -< _9
  _19 <- arr21 (-) -< {_18, _17}
  _20 <- pre1 0.7233698524596788 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10