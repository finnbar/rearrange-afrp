{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _5 <- iPre 2.0396609773968426 -< _2
  _6 <- FRP.Yampa.arr (+1) -< _1
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _3)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _5)
  _9 <- iPre 2.267615055643672 -< _8
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr11 (+1) -< _0
  _3 <- arr21 (*) -< {_0, _1}
  _4 <- arr21 (+) -< {_1, _0}
  _5 <- pre1 2.0396609773968426 -< _2
  _6 <- arr11 (+1) -< _1
  _7 <- arr21 (-) -< {_4, _3}
  _8 <- arr21 (*) -< {_7, _5}
  _9 <- pre1 2.267615055643672 -< _8
  _10 <- arr21 (+) -< {_6, _9}
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0