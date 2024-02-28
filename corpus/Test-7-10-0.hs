{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- iPre 1.3566280181568142 -< _3
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _0)
  _6 <- FRP.Yampa.arr (+1) -< _1
  _7 <- iPre 1.9446112125064159 -< _6
  _8 <- iPre 1.8257730619726016 -< _5
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _7)
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (+) -< {_1, _0}
  _3 <- arr11 (+1) -< _2
  _4 <- pre1 1.3566280181568142 -< _3
  _5 <- arr21 (+) -< {_4, _0}
  _6 <- arr11 (+1) -< _1
  _7 <- pre1 1.9446112125064159 -< _6
  _8 <- pre1 1.8257730619726016 -< _5
  _9 <- arr21 (*) -< {_8, _7}
  _10 <- arr11 (+1) -< _9
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0