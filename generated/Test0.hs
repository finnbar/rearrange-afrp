{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Int Int
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- iPre 28 -< _1
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _3)
    _5 <- iPre 4 -< _2
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _5)
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _4)
    _9 <- FRP.Yampa.arr (+1) -< _5
    _10 <- FRP.Yampa.arr (+1) -< _4
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
    _12 <- FRP.Yampa.arr (+1) -< _3
    _3 <- iPre 18 -< _11
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _7)
  _14 <- iPre 1 -< _10
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _14)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _13)
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _16)
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- iPre 10 -< _18
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Int) (V Int)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- pre1 28 -< _1
  rec
    _4 <- arr21 (-) -< {_0, _3}
    _5 <- pre1 4 -< _2
    _6 <- arr21 (-) -< {_1, _1}
    _7 <- arr21 (+) -< {_6, _5}
    _8 <- arr21 (-) -< {_1, _4}
    _9 <- arr11 (+1) -< _5
    _10 <- arr11 (+1) -< _4
    _11 <- arr21 (-) -< {_1, _3}
    _12 <- arr11 (+1) -< _3
    _3 <- pre1 18 -< _11

  _13 <- arr21 (*) -< {_12, _7}
  _14 <- pre1 1 -< _10
  _15 <- arr21 (+) -< {_8, _14}
  _16 <- arr21 (+) -< {_9, _13}
  _17 <- arr21 (*) -< {_15, _16}
  _18 <- arr11 (+1) -< _17
  _19 <- pre1 10 -< _18
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]