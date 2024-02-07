{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (+1) -< _1
  rec
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
    _3 <- iPre 12.712614505088302 -< _4
  _5 <- iPre 2.491026675752421 -< _3
  _6 <- FRP.Yampa.arr (+1) -< _4
  _7 <- iPre 8.64846580222424 -< _0
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _6)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _8)
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr11 (+1) -< _1
  rec
    _4 <- arr21 (*) -< {_3, _2}
    _3 <- pre1 12.712614505088302 -< _4

  _5 <- pre1 2.491026675752421 -< _3
  _6 <- arr11 (+1) -< _4
  _7 <- pre1 8.64846580222424 -< _0
  _8 <- arr21 (-) -< {_7, _6}
  _9 <- arr21 (*) -< {_5, _8}
  _10 <- arr11 (+1) -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2