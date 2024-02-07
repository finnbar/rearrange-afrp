{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.43723126026876763 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  rec
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _3)
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _0)
    _7 <- iPre 2.221519263648523 -< _4
    _8 <- FRP.Yampa.arr (+1) -< _0
    _9 <- FRP.Yampa.arr (+1) -< _0
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _0)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _2)
    _3 <- iPre 1.9739784301973775 -< _9
    _4 <- iPre 1.9914607839477254 -< _11
  _13 <- FRP.Yampa.arr (+1) -< _8
  _14 <- FRP.Yampa.arr (+1) -< _7
  _15 <- iPre 2.5357235536175375 -< _13
  _16 <- iPre 0.6039120059011953 -< _14
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _15)
  _18 <- FRP.Yampa.arr (+1) -< _6
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _12)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.43723126026876763 -< _0
  _2 <- arr11 (+1) -< _0
  rec
    _5 <- arr21 (*) -< {_0, _3}
    _6 <- arr21 (+) -< {_5, _0}
    _7 <- pre1 2.221519263648523 -< _4
    _8 <- arr11 (+1) -< _0
    _9 <- arr11 (+1) -< _0
    _10 <- arr21 (*) -< {_3, _0}
    _11 <- arr21 (*) -< {_1, _2}
    _12 <- arr21 (-) -< {_10, _2}
    _3 <- pre1 1.9739784301973775 -< _9
    _4 <- pre1 1.9914607839477254 -< _11

  _13 <- arr11 (+1) -< _8
  _14 <- arr11 (+1) -< _7
  _15 <- pre1 2.5357235536175375 -< _13
  _16 <- pre1 0.6039120059011953 -< _14
  _17 <- arr21 (-) -< {_16, _15}
  _18 <- arr11 (+1) -< _6
  _19 <- arr21 (-) -< {_18, _12}
  _20 <- arr21 (*) -< {_17, _19}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10