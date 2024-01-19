{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Int Int
yampa = proc _0 -> do
  _1 <- iPre 19 -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  rec
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _6 <- FRP.Yampa.arr (+1) -< _0
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _6)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _2)
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- FRP.Yampa.arr (+1) -< _9
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _6)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _8)
    _3 <- iPre 19 -< _4
    _4 <- iPre 28 -< _9
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _12)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _10)
  _15 <- iPre 15 -< _14
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _15)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _11)
  _18 <- iPre 21 -< _17
  _19 <- iPre 3 -< _18
  _20 <- iPre 20 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Int) (V Int)
afrp = [gap|proc _0 -> do
  _1 <- pre1 19 -< _0
  _2 <- arr21 (*) -< {_1, _1}
  rec
    _5 <- arr21 (+) -< {_0, _1}
    _6 <- arr11 (+1) -< _0
    _7 <- arr21 (-) -< {_5, _6}
    _8 <- arr21 (+) -< {_6, _2}
    _9 <- arr11 (+1) -< _8
    _10 <- arr11 (+1) -< _9
    _11 <- arr21 (-) -< {_0, _6}
    _12 <- arr21 (-) -< {_4, _8}
    _3 <- pre1 19 -< _4
    _4 <- pre1 28 -< _9

  _13 <- arr21 (-) -< {_7, _12}
  _14 <- arr21 (+) -< {_13, _10}
  _15 <- pre1 15 -< _14
  _16 <- arr21 (-) -< {_3, _15}
  _17 <- arr21 (+) -< {_16, _11}
  _18 <- pre1 21 -< _17
  _19 <- pre1 3 -< _18
  _20 <- pre1 20 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]