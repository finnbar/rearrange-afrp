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
    _5 <- FRP.Yampa.arr (+1) -< _0
    _6 <- FRP.Yampa.arr (+1) -< _1
    _7 <- iPre 0.6325671206353249 -< _6
    _8 <- FRP.Yampa.arr (+1) -< _0
    _9 <- FRP.Yampa.arr (+1) -< _0
    _10 <- iPre 2.0514273739463986 -< _7
    _11 <- iPre 0.28398232574299687 -< _0
    _12 <- FRP.Yampa.arr (+1) -< _0
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _8)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _3)
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _8)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _8)
    _17 <- FRP.Yampa.arr (+1) -< _2
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _10)
    _19 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _16)
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _4)
    _1 <- iPre 1.384222410346327 -< _15
    _2 <- iPre 2.253383159401655 -< _18
    _3 <- iPre 0.7929230174903868 -< _20
  FRP.Yampa.returnA -< _19

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr11 (+1) -< _0
    _5 <- arr11 (+1) -< _0
    _6 <- arr11 (+1) -< _1
    _7 <- pre1 0.6325671206353249 -< _6
    _8 <- arr11 (+1) -< _0
    _9 <- arr11 (+1) -< _0
    _10 <- pre1 2.0514273739463986 -< _7
    _11 <- pre1 0.28398232574299687 -< _0
    _12 <- arr11 (+1) -< _0
    _13 <- arr21 (+) -< {_5, _8}
    _14 <- arr21 (+) -< {_13, _3}
    _15 <- arr21 (-) -< {_14, _8}
    _16 <- arr21 (*) -< {_12, _8}
    _17 <- arr11 (+1) -< _2
    _18 <- arr21 (*) -< {_9, _10}
    _19 <- arr21 (*) -< {_17, _16}
    _20 <- arr21 (-) -< {_11, _4}
    _1 <- pre1 1.384222410346327 -< _15
    _2 <- pre1 2.253383159401655 -< _18
    _3 <- pre1 0.7929230174903868 -< _20

  GenProc.GeneralisedArrow.returnA -< _19|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20