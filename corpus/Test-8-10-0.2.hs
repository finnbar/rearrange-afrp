{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 2.490498747059979 -< _1
    _1 <- iPre 10.647057304509255 -< _2
  _3 <- FRP.Yampa.arr (+1) -< _1
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _5 <- iPre 6.335666767418854 -< _3
  _6 <- iPre 4.9574716327473185 -< _4
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (+1) -< _5
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 2.490498747059979 -< _1
    _1 <- pre1 10.647057304509255 -< _2

  _3 <- arr11 (+1) -< _1
  _4 <- arr21 (*) -< {_1, _0}
  _5 <- pre1 6.335666767418854 -< _3
  _6 <- pre1 4.9574716327473185 -< _4
  _7 <- arr11 (+1) -< _6
  _8 <- arr11 (+1) -< _5
  _9 <- arr11 (+1) -< _8
  _10 <- arr21 (*) -< {_7, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2