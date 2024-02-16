{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (+1) -< _0
    _3 <- FRP.Yampa.arr (+1) -< _0
    _4 <- FRP.Yampa.arr (+1) -< _0
    _5 <- FRP.Yampa.arr (+1) -< _4
    _1 <- iPre 0.420738258844519 -< _1
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _5)
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _2)
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _0
    _3 <- arr11 (+1) -< _0
    _4 <- arr11 (+1) -< _0
    _5 <- arr11 (+1) -< _4
    _1 <- pre1 0.420738258844519 -< _1

  _6 <- arr21 (+) -< {_3, _5}
  _7 <- arr11 (+1) -< _6
  _8 <- arr11 (+1) -< _7
  _9 <- arr21 (-) -< {_8, _2}
  _10 <- arr11 (+1) -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5