{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (+1) -< _0
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _2)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _3)
    _7 <- FRP.Yampa.arr (+1) -< _4
    _8 <- FRP.Yampa.arr (+1) -< _6
    _9 <- iPre 2.570295684343139 -< _1
    _10 <- iPre 1.323484059302355 -< _9
    _1 <- iPre 0.20838906420549774 -< _7
    _2 <- iPre 1.8271918653809445 -< _10
  FRP.Yampa.returnA -< _8

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr11 (+1) -< _0
    _4 <- arr21 (-) -< {_2, _2}
    _5 <- arr21 (+) -< {_3, _2}
    _6 <- arr21 (*) -< {_5, _3}
    _7 <- arr11 (+1) -< _4
    _8 <- arr11 (+1) -< _6
    _9 <- pre1 2.570295684343139 -< _1
    _10 <- pre1 1.323484059302355 -< _9
    _1 <- pre1 0.20838906420549774 -< _7
    _2 <- pre1 1.8271918653809445 -< _10

  GenProc.GeneralisedArrow.returnA -< _8|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10