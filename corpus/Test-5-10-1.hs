{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (+1) -< _0
    _5 <- FRP.Yampa.arr (+1) -< _0
    _6 <- iPre 1.7677982340552376 -< _0
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _1)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _7)
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _0)
    _10 <- iPre 0.45456442139681563 -< _5
    _1 <- iPre 0.9244200359661634 -< _8
    _2 <- iPre 1.084755177376364 -< _3
    _3 <- iPre 1.3446661067260972 -< _10
  FRP.Yampa.returnA -< _9

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr11 (+1) -< _0
    _5 <- arr11 (+1) -< _0
    _6 <- pre1 1.7677982340552376 -< _0
    _7 <- arr21 (*) -< {_6, _1}
    _8 <- arr21 (*) -< {_4, _7}
    _9 <- arr21 (*) -< {_2, _0}
    _10 <- pre1 0.45456442139681563 -< _5
    _1 <- pre1 0.9244200359661634 -< _8
    _2 <- pre1 1.084755177376364 -< _3
    _3 <- pre1 1.3446661067260972 -< _10

  GenProc.GeneralisedArrow.returnA -< _9|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10