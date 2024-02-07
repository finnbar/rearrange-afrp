{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- iPre 2.0435416370967716 -< _0
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
  rec
    _6 <- iPre 1.23025049133096 -< _3
    _7 <- iPre 1.6483876709774656 -< _6
    _8 <- iPre 2.2425742063385554 -< _2
    _9 <- iPre 0.6869101550626671 -< _0
    _10 <- iPre 0.8682706239800454 -< _5
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _7)
    _12 <- FRP.Yampa.arr (+1) -< _4
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _11)
    _4 <- iPre 2.637468125848718 -< _3
    _5 <- iPre 0.5213809463867162 -< _3
  _14 <- FRP.Yampa.arr (+1) -< _12
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _10)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _14)
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _8)
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- FRP.Yampa.arr (+1) -< _16
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _18)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- pre1 2.0435416370967716 -< _0
  _3 <- arr21 (-) -< {_1, _2}
  rec
    _6 <- pre1 1.23025049133096 -< _3
    _7 <- pre1 1.6483876709774656 -< _6
    _8 <- pre1 2.2425742063385554 -< _2
    _9 <- pre1 0.6869101550626671 -< _0
    _10 <- pre1 0.8682706239800454 -< _5
    _11 <- arr21 (*) -< {_6, _7}
    _12 <- arr11 (+1) -< _4
    _13 <- arr21 (+) -< {_7, _11}
    _4 <- pre1 2.637468125848718 -< _3
    _5 <- pre1 0.5213809463867162 -< _3

  _14 <- arr11 (+1) -< _12
  _15 <- arr21 (*) -< {_9, _10}
  _16 <- arr21 (+) -< {_13, _14}
  _17 <- arr21 (*) -< {_15, _8}
  _18 <- arr11 (+1) -< _17
  _19 <- arr11 (+1) -< _16
  _20 <- arr21 (*) -< {_19, _18}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10