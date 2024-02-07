{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.15121859799309334 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
  _5 <- FRP.Yampa.arr (+1) -< _0
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _4)
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _4)
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _7)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _6)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _3)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.15121859799309334 -< _0
  _2 <- arr11 (+1) -< _1
  _3 <- arr11 (+1) -< _2
  _4 <- arr21 (*) -< {_3, _2}
  _5 <- arr11 (+1) -< _0
  _6 <- arr21 (-) -< {_5, _4}
  _7 <- arr21 (+) -< {_4, _4}
  _8 <- arr21 (+) -< {_4, _7}
  _9 <- arr21 (*) -< {_8, _6}
  _10 <- arr21 (+) -< {_9, _3}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0