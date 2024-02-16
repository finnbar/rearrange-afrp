{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 4.18367146474251 -< _1
    _1 <- iPre 0.2784874513874135 -< _2
  _3 <- iPre 2.9763939396893293 -< _0
  _4 <- FRP.Yampa.arr (+1) -< _3
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
  _6 <- iPre 4.080131381753429 -< _1
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _6)
  _8 <- iPre 4.708154475834009 -< _7
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- iPre 3.3914519566526256 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 4.18367146474251 -< _1
    _1 <- pre1 0.2784874513874135 -< _2

  _3 <- pre1 2.9763939396893293 -< _0
  _4 <- arr11 (+1) -< _3
  _5 <- arr21 (+) -< {_4, _1}
  _6 <- pre1 4.080131381753429 -< _1
  _7 <- arr21 (-) -< {_5, _6}
  _8 <- pre1 4.708154475834009 -< _7
  _9 <- arr11 (+1) -< _8
  _10 <- pre1 3.3914519566526256 -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2