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
    _1 <- iPre 2.367527959187626 -< _2
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _0)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
  _5 <- iPre 2.8314702371997873 -< _2
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _5)
  _7 <- FRP.Yampa.arr (+1) -< _4
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _6)
  _10 <- iPre 1.2256489288147707 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_1, _0}
    _1 <- pre1 2.367527959187626 -< _2

  _3 <- arr21 (-) -< {_2, _0}
  _4 <- arr21 (+) -< {_0, _2}
  _5 <- pre1 2.8314702371997873 -< _2
  _6 <- arr21 (*) -< {_3, _5}
  _7 <- arr11 (+1) -< _4
  _8 <- arr11 (+1) -< _7
  _9 <- arr21 (-) -< {_8, _6}
  _10 <- pre1 1.2256489288147707 -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2