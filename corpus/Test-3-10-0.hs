{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _5 <- FRP.Yampa.arr (+1) -< _3
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _2)
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _7)
  _9 <- iPre 0.964009859300637 -< _4
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _8)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (*) -< {_1, _1}
  _3 <- arr21 (+) -< {_2, _0}
  _4 <- arr21 (*) -< {_0, _1}
  _5 <- arr11 (+1) -< _3
  _6 <- arr21 (*) -< {_1, _0}
  _7 <- arr21 (+) -< {_6, _2}
  _8 <- arr21 (+) -< {_5, _7}
  _9 <- pre1 0.964009859300637 -< _4
  _10 <- arr21 (-) -< {_9, _8}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0