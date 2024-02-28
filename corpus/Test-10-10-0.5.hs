{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _3)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
    _1 <- iPre 1.4491600657806698 -< _0
  _6 <- iPre 1.8301389252573521 -< _3
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- iPre 2.1106003779703495 -< _5
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _7)
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_0, _1}
    _3 <- arr21 (-) -< {_0, _2}
    _4 <- arr21 (*) -< {_2, _3}
    _5 <- arr21 (+) -< {_4, _1}
    _1 <- pre1 1.4491600657806698 -< _0

  _6 <- pre1 1.8301389252573521 -< _3
  _7 <- arr11 (+1) -< _6
  _8 <- pre1 2.1106003779703495 -< _5
  _9 <- arr11 (+1) -< _8
  _10 <- arr21 (-) -< {_9, _7}
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5