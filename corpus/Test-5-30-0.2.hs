{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.42824498997275273 -< _0
  _2 <- iPre 5.2418426437616274e-2 -< _1
  _3 <- iPre 0.6568438581171968 -< _2
  _4 <- iPre 0.462137090840068 -< _3
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _4)
  rec
    _9 <- FRP.Yampa.arr (+1) -< _0
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _8)
    _6 <- iPre 1.959971345626567 -< _3
    _7 <- iPre 0.15469702676871383 -< _8
    _8 <- iPre 2.9932477927420904 -< _5
  _12 <- iPre 0.6654170351459245 -< _1
  _13 <- iPre 2.392361775755913 -< _12
  _14 <- FRP.Yampa.arr (+1) -< _1
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _7)
  _16 <- iPre 0.2728014395498401 -< _8
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _6)
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _13)
  _19 <- iPre 1.6932870062550773 -< _12
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _9)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _20)
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _16)
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _15)
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _23)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _22)
  _26 <- iPre 0.3191302410266182 -< _24
  _27 <- iPre 2.4229878984625652 -< _26
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _25)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.42824498997275273 -< _0
  _2 <- pre1 5.2418426437616274e-2 -< _1
  _3 <- pre1 0.6568438581171968 -< _2
  _4 <- pre1 0.462137090840068 -< _3
  _5 <- arr21 (-) -< {_0, _4}
  rec
    _9 <- arr11 (+1) -< _0
    _10 <- arr21 (+) -< {_1, _1}
    _11 <- arr21 (+) -< {_10, _8}
    _6 <- pre1 1.959971345626567 -< _3
    _7 <- pre1 0.15469702676871383 -< _8
    _8 <- pre1 2.9932477927420904 -< _5

  _12 <- pre1 0.6654170351459245 -< _1
  _13 <- pre1 2.392361775755913 -< _12
  _14 <- arr11 (+1) -< _1
  _15 <- arr21 (-) -< {_14, _7}
  _16 <- pre1 0.2728014395498401 -< _8
  _17 <- arr21 (-) -< {_13, _6}
  _18 <- arr21 (-) -< {_17, _13}
  _19 <- pre1 1.6932870062550773 -< _12
  _20 <- arr21 (*) -< {_3, _9}
  _21 <- arr21 (+) -< {_19, _20}
  _22 <- arr21 (*) -< {_0, _16}
  _23 <- arr21 (-) -< {_11, _15}
  _24 <- arr21 (*) -< {_21, _23}
  _25 <- arr21 (-) -< {_18, _22}
  _26 <- pre1 0.3191302410266182 -< _24
  _27 <- pre1 2.4229878984625652 -< _26
  _28 <- arr21 (*) -< {_27, _25}
  _29 <- arr11 (+1) -< _28
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6