{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _5 <- FRP.Yampa.arr (+1) -< _4
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _4)
    _7 <- iPre 0.67438096038478 -< _0
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _5)
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- iPre 1.6196024686765553 -< _1
    _1 <- iPre 2.0954072482305954 -< _9
    _2 <- iPre 1.3703718745499776 -< _7
    _3 <- iPre 1.0989756249978251e-2 -< _10
    _4 <- iPre 1.8155562646663128 -< _8
  FRP.Yampa.returnA -< _2

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _5 <- arr11 (+1) -< _4
    _6 <- arr21 (*) -< {_4, _4}
    _7 <- pre1 0.67438096038478 -< _0
    _8 <- arr21 (-) -< {_3, _5}
    _9 <- arr11 (+1) -< _6
    _10 <- pre1 1.6196024686765553 -< _1
    _1 <- pre1 2.0954072482305954 -< _9
    _2 <- pre1 1.3703718745499776 -< _7
    _3 <- pre1 1.0989756249978251e-2 -< _10
    _4 <- pre1 1.8155562646663128 -< _8

  GenProc.GeneralisedArrow.returnA -< _2|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10