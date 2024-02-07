{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.1463806679385264 -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _3 <- iPre 2.595591856066982 -< _1
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
  rec
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _6)
    _8 <- iPre 1.4699573054061885 -< _4
    _5 <- iPre 0.2891496525285099 -< _4
    _6 <- iPre 2.518192147489667 -< _8
  _9 <- FRP.Yampa.arr (+1) -< _3
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _2)
  _11 <- FRP.Yampa.arr (+1) -< _5
  _12 <- FRP.Yampa.arr (+1) -< _11
  _13 <- iPre 3.131883866502882 -< _10
  _14 <- FRP.Yampa.arr (+1) -< _9
  _15 <- FRP.Yampa.arr (+1) -< _13
  _16 <- FRP.Yampa.arr (+1) -< _15
  _17 <- FRP.Yampa.arr (+1) -< _16
  _18 <- FRP.Yampa.arr (+1) -< _12
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _14)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _17)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.1463806679385264 -< _0
  _2 <- arr21 (*) -< {_1, _0}
  _3 <- pre1 2.595591856066982 -< _1
  _4 <- arr21 (*) -< {_3, _2}
  rec
    _7 <- arr21 (-) -< {_5, _6}
    _8 <- pre1 1.4699573054061885 -< _4
    _5 <- pre1 0.2891496525285099 -< _4
    _6 <- pre1 2.518192147489667 -< _8

  _9 <- arr11 (+1) -< _3
  _10 <- arr21 (*) -< {_7, _2}
  _11 <- arr11 (+1) -< _5
  _12 <- arr11 (+1) -< _11
  _13 <- pre1 3.131883866502882 -< _10
  _14 <- arr11 (+1) -< _9
  _15 <- arr11 (+1) -< _13
  _16 <- arr11 (+1) -< _15
  _17 <- arr11 (+1) -< _16
  _18 <- arr11 (+1) -< _12
  _19 <- arr21 (*) -< {_18, _14}
  _20 <- arr21 (-) -< {_19, _17}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4