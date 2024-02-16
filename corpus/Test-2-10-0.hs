{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.5211901415345457 -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _3)
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _3)
  _6 <- FRP.Yampa.arr (+1) -< _4
  _7 <- iPre 0.9724998588707587 -< _5
  _8 <- FRP.Yampa.arr (+1) -< _6
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _8)
  _10 <- iPre 2.001813497946196 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.5211901415345457 -< _0
  _2 <- arr21 (*) -< {_1, _0}
  _3 <- arr11 (+1) -< _2
  _4 <- arr21 (*) -< {_0, _3}
  _5 <- arr21 (*) -< {_1, _3}
  _6 <- arr11 (+1) -< _4
  _7 <- pre1 0.9724998588707587 -< _5
  _8 <- arr11 (+1) -< _6
  _9 <- arr21 (-) -< {_7, _8}
  _10 <- pre1 2.001813497946196 -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0