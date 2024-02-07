{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  rec
    _4 <- iPre 2.206955597974906 -< _3
    _3 <- iPre 3.4079073980102024 -< _4
  _5 <- FRP.Yampa.arr (+1) -< _4
  _6 <- iPre 1.0472263793998513 -< _2
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _5)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (-) -< {_0, _0}
  rec
    _4 <- pre1 2.206955597974906 -< _3
    _3 <- pre1 3.4079073980102024 -< _4

  _5 <- arr11 (+1) -< _4
  _6 <- pre1 1.0472263793998513 -< _2
  _7 <- arr11 (+1) -< _6
  _8 <- arr11 (+1) -< _7
  _9 <- arr21 (+) -< {_1, _5}
  _10 <- arr21 (*) -< {_8, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2