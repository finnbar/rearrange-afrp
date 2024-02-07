{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _5 <- iPre 2.6648384334778865 -< _1
    _6 <- iPre 1.7081880488650234 -< _1
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _8)
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _9)
    _1 <- iPre 2.1160138728772515 -< _6
    _2 <- iPre 0.85254879634679 -< _10
    _3 <- iPre 2.455371651820351 -< _5
    _4 <- iPre 1.0003286918263012 -< _0
  FRP.Yampa.returnA -< _7

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _5 <- pre1 2.6648384334778865 -< _1
    _6 <- pre1 1.7081880488650234 -< _1
    _7 <- arr21 (-) -< {_1, _3}
    _8 <- arr21 (-) -< {_1, _1}
    _9 <- arr21 (*) -< {_2, _8}
    _10 <- arr21 (-) -< {_4, _9}
    _1 <- pre1 2.1160138728772515 -< _6
    _2 <- pre1 0.85254879634679 -< _10
    _3 <- pre1 2.455371651820351 -< _5
    _4 <- pre1 1.0003286918263012 -< _0

  GenProc.GeneralisedArrow.returnA -< _7|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10