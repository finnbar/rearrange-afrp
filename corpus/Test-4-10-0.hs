{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
  _4 <- iPre 0.2242452568814559 -< _3
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _4)
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _5)
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _4)
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _8)
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- arr21 (+) -< {_0, _1}
  _4 <- pre1 0.2242452568814559 -< _3
  _5 <- arr21 (*) -< {_0, _4}
  _6 <- arr21 (*) -< {_4, _5}
  _7 <- arr21 (+) -< {_6, _4}
  _8 <- arr11 (+1) -< _7
  _9 <- arr21 (-) -< {_2, _8}
  _10 <- arr11 (+1) -< _9
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0