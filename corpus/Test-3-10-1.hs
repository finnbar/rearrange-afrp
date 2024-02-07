{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
    _5 <- FRP.Yampa.arr (+1) -< _3
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _3)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _4)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _5)
    _9 <- iPre 0.771263563948071 -< _2
    _10 <- FRP.Yampa.arr (+1) -< _7
    _1 <- iPre 0.9836131321888524 -< _8
    _2 <- iPre 0.5470888763661664 -< _6
    _3 <- iPre 0.5001379425666161 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr21 (-) -< {_0, _1}
    _5 <- arr11 (+1) -< _3
    _6 <- arr21 (-) -< {_3, _3}
    _7 <- arr21 (*) -< {_2, _4}
    _8 <- arr21 (*) -< {_3, _5}
    _9 <- pre1 0.771263563948071 -< _2
    _10 <- arr11 (+1) -< _7
    _1 <- pre1 0.9836131321888524 -< _8
    _2 <- pre1 0.5470888763661664 -< _6
    _3 <- pre1 0.5001379425666161 -< _9

  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10