{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Int Int
yampa = proc _0 -> do
  _1 <- iPre 19 -< _0
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _4)
    _6 <- iPre 2 -< _4
    _7 <- FRP.Yampa.arr (+1) -< _5
    _8 <- FRP.Yampa.arr (+1) -< _6
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _8)
    _10 <- iPre 2 -< _9
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _10)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _7)
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _4)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _1)
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _4)
    _16 <- iPre 26 -< _14
    _17 <- FRP.Yampa.arr (+1) -< _16
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _15)
    _19 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _18)
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _0)
    _21 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _12)
    _2 <- iPre 9 -< _8
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _19)
  _23 <- iPre 11 -< _6
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _10)
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _24)
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _21)
  _27 <- iPre 0 -< _26
  _28 <- iPre 22 -< _27
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _25)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Int) (V Int)
afrp = [gap|proc _0 -> do
  _1 <- pre1 19 -< _0
  rec
    _3 <- arr21 (-) -< {_0, _1}
    _4 <- arr11 (+1) -< _1
    _5 <- arr21 (+) -< {_2, _4}
    _6 <- pre1 2 -< _4
    _7 <- arr11 (+1) -< _5
    _8 <- arr11 (+1) -< _6
    _9 <- arr21 (-) -< {_3, _8}
    _10 <- pre1 2 -< _9
    _11 <- arr21 (*) -< {_7, _10}
    _12 <- arr21 (-) -< {_11, _7}
    _13 <- arr21 (*) -< {_12, _4}
    _14 <- arr21 (+) -< {_13, _1}
    _15 <- arr21 (+) -< {_11, _4}
    _16 <- pre1 26 -< _14
    _17 <- arr11 (+1) -< _16
    _18 <- arr21 (+) -< {_15, _15}
    _19 <- arr21 (*) -< {_17, _18}
    _20 <- arr21 (-) -< {_2, _0}
    _21 <- arr21 (+) -< {_20, _12}
    _2 <- pre1 9 -< _8

  _22 <- arr21 (*) -< {_5, _19}
  _23 <- pre1 11 -< _6
  _24 <- arr21 (+) -< {_11, _10}
  _25 <- arr21 (*) -< {_22, _24}
  _26 <- arr21 (*) -< {_23, _21}
  _27 <- pre1 0 -< _26
  _28 <- pre1 22 -< _27
  _29 <- arr21 (+) -< {_20, _25}
  _30 <- arr21 (-) -< {_28, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 20