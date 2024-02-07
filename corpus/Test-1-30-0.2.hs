{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  rec
    _4 <- FRP.Yampa.arr (+1) -< _0
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _3)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _3)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _2)
    _2 <- iPre 2.1333173284549902 -< _6
    _3 <- iPre 0.5905133061628579 -< _2
  _8 <- iPre 2.6838804476633973 -< _3
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _1)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _8)
  _11 <- FRP.Yampa.arr (+1) -< _5
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _9)
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _5)
  _14 <- FRP.Yampa.arr (+1) -< _10
  _15 <- FRP.Yampa.arr (+1) -< _14
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _15)
  _17 <- iPre 0.9234556179701618 -< _8
  _18 <- FRP.Yampa.arr (+1) -< _12
  _19 <- FRP.Yampa.arr (+1) -< _11
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _18)
  _21 <- FRP.Yampa.arr (+1) -< _4
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _21)
  _23 <- FRP.Yampa.arr (+1) -< _16
  _24 <- FRP.Yampa.arr (+1) -< _7
  _25 <- FRP.Yampa.arr (+1) -< _22
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _25)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _24)
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _27)
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _26)
  _30 <- iPre 2.2780632485641226 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  rec
    _4 <- arr11 (+1) -< _0
    _5 <- arr21 (*) -< {_0, _3}
    _6 <- arr21 (-) -< {_4, _3}
    _7 <- arr21 (*) -< {_2, _2}
    _2 <- pre1 2.1333173284549902 -< _6
    _3 <- pre1 0.5905133061628579 -< _2

  _8 <- pre1 2.6838804476633973 -< _3
  _9 <- arr21 (+) -< {_5, _1}
  _10 <- arr21 (*) -< {_7, _8}
  _11 <- arr11 (+1) -< _5
  _12 <- arr21 (-) -< {_1, _9}
  _13 <- arr21 (-) -< {_7, _5}
  _14 <- arr11 (+1) -< _10
  _15 <- arr11 (+1) -< _14
  _16 <- arr21 (-) -< {_13, _15}
  _17 <- pre1 0.9234556179701618 -< _8
  _18 <- arr11 (+1) -< _12
  _19 <- arr11 (+1) -< _11
  _20 <- arr21 (+) -< {_13, _18}
  _21 <- arr11 (+1) -< _4
  _22 <- arr21 (+) -< {_19, _21}
  _23 <- arr11 (+1) -< _16
  _24 <- arr11 (+1) -< _7
  _25 <- arr11 (+1) -< _22
  _26 <- arr21 (-) -< {_20, _25}
  _27 <- arr21 (+) -< {_23, _24}
  _28 <- arr21 (+) -< {_17, _27}
  _29 <- arr21 (+) -< {_28, _26}
  _30 <- pre1 2.2780632485641226 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6