{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.2257150226770586 -< _0
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (+1) -< _1
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
  rec
    _6 <- iPre 0.45470266141851057 -< _5
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _4)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _6)
    _9 <- FRP.Yampa.arr (+1) -< _3
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _8)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _9)
    _12 <- FRP.Yampa.arr (+1) -< _2
    _13 <- FRP.Yampa.arr (+1) -< _5
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _11)
    _15 <- iPre 0.4557925797351042 -< _3
    _16 <- FRP.Yampa.arr (+1) -< _13
    _17 <- FRP.Yampa.arr (+1) -< _12
    _18 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _16)
    _19 <- FRP.Yampa.arr (+1) -< _18
    _5 <- iPre 1.4158018991287218 -< _11
  _20 <- iPre 0.9826953286038987 -< _19
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _20)
  _22 <- iPre 1.7661062495019466 -< _7
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _14)
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _22)
  _25 <- iPre 1.7658384632896058 -< _24
  _26 <- FRP.Yampa.arr (+1) -< _25
  _27 <- FRP.Yampa.arr (+1) -< _26
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- iPre 2.1090624371740243 -< _28
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.2257150226770586 -< _0
  _2 <- arr21 (+) -< {_1, _0}
  _3 <- arr11 (+1) -< _1
  _4 <- arr21 (-) -< {_2, _2}
  rec
    _6 <- pre1 0.45470266141851057 -< _5
    _7 <- arr21 (*) -< {_1, _4}
    _8 <- arr21 (*) -< {_4, _6}
    _9 <- arr11 (+1) -< _3
    _10 <- arr21 (-) -< {_1, _8}
    _11 <- arr21 (*) -< {_10, _9}
    _12 <- arr11 (+1) -< _2
    _13 <- arr11 (+1) -< _5
    _14 <- arr21 (+) -< {_1, _11}
    _15 <- pre1 0.4557925797351042 -< _3
    _16 <- arr11 (+1) -< _13
    _17 <- arr11 (+1) -< _12
    _18 <- arr21 (-) -< {_17, _16}
    _19 <- arr11 (+1) -< _18
    _5 <- pre1 1.4158018991287218 -< _11

  _20 <- pre1 0.9826953286038987 -< _19
  _21 <- arr21 (+) -< {_15, _20}
  _22 <- pre1 1.7661062495019466 -< _7
  _23 <- arr21 (-) -< {_21, _14}
  _24 <- arr21 (+) -< {_23, _22}
  _25 <- pre1 1.7658384632896058 -< _24
  _26 <- arr11 (+1) -< _25
  _27 <- arr11 (+1) -< _26
  _28 <- arr11 (+1) -< _27
  _29 <- pre1 2.1090624371740243 -< _28
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15