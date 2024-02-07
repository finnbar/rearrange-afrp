{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  rec
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _6)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _7)
    _12 <- FRP.Yampa.arr (+1) -< _11
    _13 <- iPre 2.4069027407795405 -< _0
    _14 <- FRP.Yampa.arr (+1) -< _10
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _8)
    _16 <- iPre 1.5030228679859434 -< _11
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _13)
    _18 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _16)
    _19 <- FRP.Yampa.arr (+1) -< _18
    _5 <- iPre 2.0720841061037523 -< _14
    _6 <- iPre 1.9527060702670787 -< _15
    _7 <- iPre 0.27509239660051593 -< _17
    _8 <- iPre 1.463311004345572 -< _2
  _20 <- iPre 1.4656704013063808 -< _9
  _21 <- FRP.Yampa.arr (+1) -< _20
  _22 <- FRP.Yampa.arr (+1) -< _21
  _23 <- FRP.Yampa.arr (+1) -< _22
  _24 <- iPre 2.132614087538354 -< _23
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _19)
  _26 <- iPre 0.48270561891482305 -< _25
  _27 <- FRP.Yampa.arr (+1) -< _26
  _28 <- iPre 1.8359236922910038 -< _27
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- arr21 (+) -< {_0, _1}
  _4 <- arr21 (*) -< {_1, _1}
  rec
    _9 <- arr21 (+) -< {_0, _6}
    _10 <- arr21 (+) -< {_4, _1}
    _11 <- arr21 (*) -< {_7, _7}
    _12 <- arr11 (+1) -< _11
    _13 <- pre1 2.4069027407795405 -< _0
    _14 <- arr11 (+1) -< _10
    _15 <- arr21 (+) -< {_12, _8}
    _16 <- pre1 1.5030228679859434 -< _11
    _17 <- arr21 (+) -< {_5, _13}
    _18 <- arr21 (-) -< {_3, _16}
    _19 <- arr11 (+1) -< _18
    _5 <- pre1 2.0720841061037523 -< _14
    _6 <- pre1 1.9527060702670787 -< _15
    _7 <- pre1 0.27509239660051593 -< _17
    _8 <- pre1 1.463311004345572 -< _2

  _20 <- pre1 1.4656704013063808 -< _9
  _21 <- arr11 (+1) -< _20
  _22 <- arr11 (+1) -< _21
  _23 <- arr11 (+1) -< _22
  _24 <- pre1 2.132614087538354 -< _23
  _25 <- arr21 (*) -< {_24, _19}
  _26 <- pre1 0.48270561891482305 -< _25
  _27 <- arr11 (+1) -< _26
  _28 <- pre1 1.8359236922910038 -< _27
  _29 <- arr11 (+1) -< _28
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15