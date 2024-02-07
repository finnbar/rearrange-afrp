{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- iPre 1.0837689275074573 -< _2
    _4 <- iPre 2.958029578322614 -< _2
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _3)
    _1 <- iPre 1.9573477260505754 -< _5
    _2 <- iPre 0.9358373622221674 -< _0
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _4)
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _2)
  _8 <- FRP.Yampa.arr (+1) -< _6
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _7)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _8)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- pre1 1.0837689275074573 -< _2
    _4 <- pre1 2.958029578322614 -< _2
    _5 <- arr21 (+) -< {_2, _3}
    _1 <- pre1 1.9573477260505754 -< _5
    _2 <- pre1 0.9358373622221674 -< _0

  _6 <- arr21 (*) -< {_5, _4}
  _7 <- arr21 (-) -< {_4, _2}
  _8 <- arr11 (+1) -< _6
  _9 <- arr21 (-) -< {_1, _7}
  _10 <- arr21 (-) -< {_9, _8}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5