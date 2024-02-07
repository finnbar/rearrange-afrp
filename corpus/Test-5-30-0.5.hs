{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (+1) -< _0
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _6 <- FRP.Yampa.arr (+1) -< _0
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _9 <- FRP.Yampa.arr (+1) -< _7
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _3)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _7)
    _12 <- FRP.Yampa.arr (+1) -< _6
    _13 <- iPre 0.8125045734706482 -< _7
    _14 <- FRP.Yampa.arr (+1) -< _7
    _15 <- FRP.Yampa.arr (+1) -< _4
    _1 <- iPre 0.6299887075223474 -< _12
    _2 <- iPre 0.9160244477899459 -< _9
    _3 <- iPre 0.346288092387714 -< _8
  _16 <- FRP.Yampa.arr (+1) -< _11
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _11)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _1)
  _19 <- iPre 1.387089412976444 -< _17
  _20 <- iPre 2.402391546752758 -< _14
  _21 <- FRP.Yampa.arr (+1) -< _2
  _22 <- FRP.Yampa.arr (+1) -< _21
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _22)
  _24 <- iPre 1.5514266803732517 -< _18
  _25 <- iPre 0.22953041178807515 -< _24
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _19)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _26)
  _28 <- FRP.Yampa.arr (+1) -< _20
  _29 <- FRP.Yampa.arr (+1) -< _27
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _28)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr11 (+1) -< _0
    _5 <- arr21 (+) -< {_0, _0}
    _6 <- arr11 (+1) -< _0
    _7 <- arr21 (*) -< {_0, _0}
    _8 <- arr21 (*) -< {_0, _0}
    _9 <- arr11 (+1) -< _7
    _10 <- arr21 (-) -< {_5, _3}
    _11 <- arr21 (*) -< {_10, _7}
    _12 <- arr11 (+1) -< _6
    _13 <- pre1 0.8125045734706482 -< _7
    _14 <- arr11 (+1) -< _7
    _15 <- arr11 (+1) -< _4
    _1 <- pre1 0.6299887075223474 -< _12
    _2 <- pre1 0.9160244477899459 -< _9
    _3 <- pre1 0.346288092387714 -< _8

  _16 <- arr11 (+1) -< _11
  _17 <- arr21 (*) -< {_13, _11}
  _18 <- arr21 (*) -< {_16, _1}
  _19 <- pre1 1.387089412976444 -< _17
  _20 <- pre1 2.402391546752758 -< _14
  _21 <- arr11 (+1) -< _2
  _22 <- arr11 (+1) -< _21
  _23 <- arr21 (+) -< {_15, _22}
  _24 <- pre1 1.5514266803732517 -< _18
  _25 <- pre1 0.22953041178807515 -< _24
  _26 <- arr21 (*) -< {_25, _19}
  _27 <- arr21 (+) -< {_23, _26}
  _28 <- arr11 (+1) -< _20
  _29 <- arr11 (+1) -< _27
  _30 <- arr21 (-) -< {_29, _28}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15