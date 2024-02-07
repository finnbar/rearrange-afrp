{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (+1) -< _2
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _0)
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _0)
    _1 <- iPre 1.1061925735177651 -< _0
    _2 <- iPre 2.674243536624526 -< _1
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _3)
  _7 <- iPre 2.5329152809837483 -< _5
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _6)
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr11 (+1) -< _2
    _4 <- arr21 (-) -< {_2, _0}
    _5 <- arr21 (*) -< {_4, _0}
    _1 <- pre1 1.1061925735177651 -< _0
    _2 <- pre1 2.674243536624526 -< _1

  _6 <- arr21 (+) -< {_2, _3}
  _7 <- pre1 2.5329152809837483 -< _5
  _8 <- arr11 (+1) -< _7
  _9 <- arr21 (-) -< {_8, _6}
  _10 <- arr11 (+1) -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5