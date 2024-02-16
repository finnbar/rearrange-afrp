{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.664394606894217 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- iPre 1.8346197777811182 -< _1
  _4 <- FRP.Yampa.arr (+1) -< _2
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _6 <- iPre 2.0314615996257603 -< _3
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
  rec
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _8)
    _11 <- FRP.Yampa.arr (+1) -< _6
    _12 <- iPre 2.8460791553216316 -< _7
    _13 <- iPre 2.6960756237876784 -< _1
    _8 <- iPre 2.6926252783324323 -< _11
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _13)
  _15 <- iPre 2.540171433766733 -< _0
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _4)
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _5)
  _18 <- FRP.Yampa.arr (+1) -< _16
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _9)
  _20 <- iPre 0.256989227756585 -< _12
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _17)
  _22 <- FRP.Yampa.arr (+1) -< _20
  _23 <- iPre 2.5473869220144976 -< _19
  _24 <- FRP.Yampa.arr (+1) -< _22
  _25 <- FRP.Yampa.arr (+1) -< _24
  _26 <- iPre 1.4852054889531554 -< _23
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _21)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _15)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.664394606894217 -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- pre1 1.8346197777811182 -< _1
  _4 <- arr11 (+1) -< _2
  _5 <- arr21 (*) -< {_0, _0}
  _6 <- pre1 2.0314615996257603 -< _3
  _7 <- arr21 (-) -< {_2, _1}
  rec
    _9 <- arr21 (+) -< {_0, _2}
    _10 <- arr21 (*) -< {_2, _8}
    _11 <- arr11 (+1) -< _6
    _12 <- pre1 2.8460791553216316 -< _7
    _13 <- pre1 2.6960756237876784 -< _1
    _8 <- pre1 2.6926252783324323 -< _11

  _14 <- arr21 (*) -< {_10, _13}
  _15 <- pre1 2.540171433766733 -< _0
  _16 <- arr21 (-) -< {_11, _4}
  _17 <- arr21 (*) -< {_6, _5}
  _18 <- arr11 (+1) -< _16
  _19 <- arr21 (+) -< {_14, _9}
  _20 <- pre1 0.256989227756585 -< _12
  _21 <- arr21 (-) -< {_18, _17}
  _22 <- arr11 (+1) -< _20
  _23 <- pre1 2.5473869220144976 -< _19
  _24 <- arr11 (+1) -< _22
  _25 <- arr11 (+1) -< _24
  _26 <- pre1 1.4852054889531554 -< _23
  _27 <- arr21 (*) -< {_26, _21}
  _28 <- arr21 (*) -< {_27, _15}
  _29 <- arr11 (+1) -< _28
  _30 <- arr21 (-) -< {_25, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6