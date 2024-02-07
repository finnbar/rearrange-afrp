{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- iPre 1.7786631277566094 -< _1
  _5 <- FRP.Yampa.arr (+1) -< _3
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _5)
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (+1) -< _4
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _8)
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr11 (+1) -< _0
  _3 <- arr11 (+1) -< _2
  _4 <- pre1 1.7786631277566094 -< _1
  _5 <- arr11 (+1) -< _3
  _6 <- arr21 (*) -< {_3, _5}
  _7 <- arr11 (+1) -< _6
  _8 <- arr11 (+1) -< _4
  _9 <- arr21 (*) -< {_7, _8}
  _10 <- arr11 (+1) -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0