{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _3 <- iPre 0.1344097258554619 -< _1
  _4 <- iPre 0.3200435769564377 -< _2
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _4)
  _6 <- iPre 0.8843749192693906 -< _0
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _6)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _3)
  _9 <- iPre 0.4213826214189573 -< _8
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _3)
  _11 <- FRP.Yampa.arr (+1) -< _9
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _6)
  _13 <- FRP.Yampa.arr (+1) -< _10
  _14 <- FRP.Yampa.arr (+1) -< _12
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _8)
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _16)
  _18 <- FRP.Yampa.arr (+1) -< _13
  _19 <- iPre 1.39130422514573 -< _18
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _17)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (-) -< {_0, _0}
  _3 <- pre1 0.1344097258554619 -< _1
  _4 <- pre1 0.3200435769564377 -< _2
  _5 <- arr21 (+) -< {_4, _4}
  _6 <- pre1 0.8843749192693906 -< _0
  _7 <- arr21 (*) -< {_5, _6}
  _8 <- arr21 (*) -< {_7, _3}
  _9 <- pre1 0.4213826214189573 -< _8
  _10 <- arr21 (-) -< {_0, _3}
  _11 <- arr11 (+1) -< _9
  _12 <- arr21 (+) -< {_11, _6}
  _13 <- arr11 (+1) -< _10
  _14 <- arr11 (+1) -< _12
  _15 <- arr21 (+) -< {_8, _8}
  _16 <- arr11 (+1) -< _14
  _17 <- arr21 (-) -< {_15, _16}
  _18 <- arr11 (+1) -< _13
  _19 <- pre1 1.39130422514573 -< _18
  _20 <- arr21 (*) -< {_19, _17}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0