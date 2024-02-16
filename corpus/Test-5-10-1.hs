{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 2.98807877619951 -< _0
    _3 <- FRP.Yampa.arr (+1) -< _0
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _3)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
    _7 <- FRP.Yampa.arr (+1) -< _6
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _7)
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- iPre 1.2324093757861754 -< _5
    _1 <- iPre 2.206041602726847 -< _10
  FRP.Yampa.returnA -< _9

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 2.98807877619951 -< _0
    _3 <- arr11 (+1) -< _0
    _4 <- arr11 (+1) -< _1
    _5 <- arr21 (*) -< {_4, _3}
    _6 <- arr21 (-) -< {_0, _2}
    _7 <- arr11 (+1) -< _6
    _8 <- arr21 (+) -< {_2, _7}
    _9 <- arr11 (+1) -< _8
    _10 <- pre1 1.2324093757861754 -< _5
    _1 <- pre1 2.206041602726847 -< _10

  GenProc.GeneralisedArrow.returnA -< _9|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10