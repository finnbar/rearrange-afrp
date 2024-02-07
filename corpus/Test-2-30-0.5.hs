{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (+1) -< _2
  rec
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _3)
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _0)
    _10 <- FRP.Yampa.arr (+1) -< _0
    _11 <- iPre 1.605502130195958 -< _2
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _11)
    _13 <- FRP.Yampa.arr (+1) -< _12
    _14 <- FRP.Yampa.arr (+1) -< _6
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _3)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _7)
    _17 <- FRP.Yampa.arr (+1) -< _12
    _18 <- FRP.Yampa.arr (+1) -< _13
    _4 <- iPre 0.5381177676537255 -< _10
    _5 <- iPre 2.7913906573159233 -< _14
    _6 <- iPre 1.0782647758601287 -< _15
    _7 <- iPre 0.6511037015214629 -< _8
  _19 <- iPre 1.5889036662846507 -< _16
  _20 <- FRP.Yampa.arr (+1) -< _17
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _18)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _21)
  _23 <- FRP.Yampa.arr (+1) -< _19
  _24 <- iPre 2.222157353726864 -< _23
  _25 <- FRP.Yampa.arr (+1) -< _22
  _26 <- iPre 1.3820815611720911 -< _24
  _27 <- FRP.Yampa.arr (+1) -< _25
  _28 <- FRP.Yampa.arr (+1) -< _26
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _28)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (+) -< {_1, _1}
  _3 <- arr11 (+1) -< _2
  rec
    _8 <- arr21 (*) -< {_7, _3}
    _9 <- arr21 (*) -< {_5, _0}
    _10 <- arr11 (+1) -< _0
    _11 <- pre1 1.605502130195958 -< _2
    _12 <- arr21 (+) -< {_2, _11}
    _13 <- arr11 (+1) -< _12
    _14 <- arr11 (+1) -< _6
    _15 <- arr21 (+) -< {_11, _3}
    _16 <- arr21 (*) -< {_5, _7}
    _17 <- arr11 (+1) -< _12
    _18 <- arr11 (+1) -< _13
    _4 <- pre1 0.5381177676537255 -< _10
    _5 <- pre1 2.7913906573159233 -< _14
    _6 <- pre1 1.0782647758601287 -< _15
    _7 <- pre1 0.6511037015214629 -< _8

  _19 <- pre1 1.5889036662846507 -< _16
  _20 <- arr11 (+1) -< _17
  _21 <- arr21 (+) -< {_20, _18}
  _22 <- arr21 (-) -< {_9, _21}
  _23 <- arr11 (+1) -< _19
  _24 <- pre1 2.222157353726864 -< _23
  _25 <- arr11 (+1) -< _22
  _26 <- pre1 1.3820815611720911 -< _24
  _27 <- arr11 (+1) -< _25
  _28 <- arr11 (+1) -< _26
  _29 <- arr21 (*) -< {_27, _28}
  _30 <- arr21 (-) -< {_4, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15