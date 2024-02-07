{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _4 <- FRP.Yampa.arr (+1) -< _0
    _1 <- iPre 0.9490832335733956 -< _3
    _2 <- iPre 0.8400039276377862 -< _2
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
  _6 <- FRP.Yampa.arr (+1) -< _5
  _7 <- iPre 1.8584838402727384 -< _6
  _8 <- iPre 2.7374674225541633 -< _2
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- iPre 1.6188321655328133 -< _5
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _3)
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _11)
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _2)
  _14 <- FRP.Yampa.arr (+1) -< _3
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _13)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _10)
  _17 <- FRP.Yampa.arr (+1) -< _12
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _17)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _15)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _18)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr21 (+) -< {_0, _0}
    _4 <- arr11 (+1) -< _0
    _1 <- pre1 0.9490832335733956 -< _3
    _2 <- pre1 0.8400039276377862 -< _2

  _5 <- arr21 (-) -< {_1, _2}
  _6 <- arr11 (+1) -< _5
  _7 <- pre1 1.8584838402727384 -< _6
  _8 <- pre1 2.7374674225541633 -< _2
  _9 <- arr11 (+1) -< _8
  _10 <- pre1 1.6188321655328133 -< _5
  _11 <- arr21 (+) -< {_7, _3}
  _12 <- arr21 (*) -< {_7, _11}
  _13 <- arr21 (-) -< {_8, _2}
  _14 <- arr11 (+1) -< _3
  _15 <- arr21 (*) -< {_14, _13}
  _16 <- arr21 (*) -< {_4, _10}
  _17 <- arr11 (+1) -< _12
  _18 <- arr21 (*) -< {_16, _17}
  _19 <- arr21 (*) -< {_9, _15}
  _20 <- arr21 (+) -< {_19, _18}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4