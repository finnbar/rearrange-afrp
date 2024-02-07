{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  rec
    _3 <- FRP.Yampa.arr (+1) -< _2
    _2 <- iPre 7.289071154113092 -< _3
  _4 <- iPre 19.355456491313625 -< _2
  _5 <- iPre 12.727519438101176 -< _1
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _4)
  _7 <- iPre 4.399763051291236 -< _5
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _6)
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  rec
    _3 <- arr11 (+1) -< _2
    _2 <- pre1 7.289071154113092 -< _3

  _4 <- pre1 19.355456491313625 -< _2
  _5 <- pre1 12.727519438101176 -< _1
  _6 <- arr21 (+) -< {_4, _4}
  _7 <- pre1 4.399763051291236 -< _5
  _8 <- arr21 (*) -< {_7, _6}
  _9 <- arr11 (+1) -< _8
  _10 <- arr11 (+1) -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2