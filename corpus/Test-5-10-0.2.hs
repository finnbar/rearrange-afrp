{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
    _1 <- iPre 1.0407153903456619 -< _2
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  _4 <- FRP.Yampa.arr (+1) -< _2
  _5 <- FRP.Yampa.arr (+1) -< _1
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _1)
  _7 <- iPre 1.1989079393154376 -< _6
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _4)
  _9 <- iPre 3.3987045313654503 -< _7
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _8)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_1, _0}
    _1 <- pre1 1.0407153903456619 -< _2

  _3 <- arr21 (+) -< {_1, _1}
  _4 <- arr11 (+1) -< _2
  _5 <- arr11 (+1) -< _1
  _6 <- arr21 (*) -< {_3, _1}
  _7 <- pre1 1.1989079393154376 -< _6
  _8 <- arr21 (*) -< {_5, _4}
  _9 <- pre1 3.3987045313654503 -< _7
  _10 <- arr21 (+) -< {_9, _8}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2