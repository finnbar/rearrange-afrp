{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
  _5 <- iPre 2.371186749241591 -< _3
  _6 <- iPre 2.52365280957261 -< _0
  _7 <- iPre 0.35887629908154756 -< _0
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _7)
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _6)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _8)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- arr21 (+) -< {_0, _2}
  _4 <- arr21 (+) -< {_2, _2}
  _5 <- pre1 2.371186749241591 -< _3
  _6 <- pre1 2.52365280957261 -< _0
  _7 <- pre1 0.35887629908154756 -< _0
  _8 <- arr21 (-) -< {_5, _7}
  _9 <- arr21 (-) -< {_4, _6}
  _10 <- arr21 (*) -< {_9, _8}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0