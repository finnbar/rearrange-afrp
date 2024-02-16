{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.4854984183620161 -< _0
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _5 <- FRP.Yampa.arr (+1) -< _0
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _1)
    _7 <- iPre 3.222990716720092 -< _5
    _8 <- iPre 1.9062534724381606 -< _1
    _9 <- FRP.Yampa.arr (+1) -< _0
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _7)
    _11 <- FRP.Yampa.arr (+1) -< _2
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _11)
    _13 <- FRP.Yampa.arr (+1) -< _11
    _14 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _3)
    _15 <- iPre 0.3774808464095897 -< _7
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _8)
    _2 <- iPre 1.619183468490977 -< _6
  _17 <- iPre 2.4297927382979694 -< _8
  _18 <- FRP.Yampa.arr (+1) -< _13
  _19 <- FRP.Yampa.arr (+1) -< _6
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _16)
  _21 <- FRP.Yampa.arr (+1) -< _12
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _14)
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _15)
  _24 <- iPre 0.14204184389334554 -< _23
  _25 <- iPre 3.092316700627764 -< _19
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _24)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _22)
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _9)
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _25)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.4854984183620161 -< _0
  rec
    _3 <- arr21 (-) -< {_0, _0}
    _4 <- arr21 (*) -< {_0, _0}
    _5 <- arr11 (+1) -< _0
    _6 <- arr21 (*) -< {_4, _1}
    _7 <- pre1 3.222990716720092 -< _5
    _8 <- pre1 1.9062534724381606 -< _1
    _9 <- arr11 (+1) -< _0
    _10 <- arr21 (+) -< {_4, _7}
    _11 <- arr11 (+1) -< _2
    _12 <- arr21 (*) -< {_1, _11}
    _13 <- arr11 (+1) -< _11
    _14 <- arr21 (-) -< {_11, _3}
    _15 <- pre1 0.3774808464095897 -< _7
    _16 <- arr21 (-) -< {_10, _8}
    _2 <- pre1 1.619183468490977 -< _6

  _17 <- pre1 2.4297927382979694 -< _8
  _18 <- arr11 (+1) -< _13
  _19 <- arr11 (+1) -< _6
  _20 <- arr21 (+) -< {_10, _16}
  _21 <- arr11 (+1) -< _12
  _22 <- arr21 (*) -< {_17, _14}
  _23 <- arr21 (-) -< {_21, _15}
  _24 <- pre1 0.14204184389334554 -< _23
  _25 <- pre1 3.092316700627764 -< _19
  _26 <- arr21 (+) -< {_18, _24}
  _27 <- arr21 (+) -< {_20, _22}
  _28 <- arr21 (+) -< {_26, _9}
  _29 <- arr21 (-) -< {_28, _25}
  _30 <- arr21 (*) -< {_27, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15