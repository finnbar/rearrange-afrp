{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
    _3 <- iPre 4.682542632461572 -< _4
  _5 <- iPre 3.9467475627759603 -< _4
  _6 <- iPre 3.11563084771468 -< _3
  _7 <- iPre 3.4237862028245045 -< _5
  _8 <- iPre 0.4099149969942544 -< _2
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _8)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (+) -< {_1, _1}
  rec
    _4 <- arr21 (-) -< {_2, _3}
    _3 <- pre1 4.682542632461572 -< _4

  _5 <- pre1 3.9467475627759603 -< _4
  _6 <- pre1 3.11563084771468 -< _3
  _7 <- pre1 3.4237862028245045 -< _5
  _8 <- pre1 0.4099149969942544 -< _2
  _9 <- arr21 (-) -< {_7, _8}
  _10 <- arr21 (+) -< {_6, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2