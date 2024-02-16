{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
    _4 <- FRP.Yampa.arr (+1) -< _2
    _5 <- FRP.Yampa.arr (+1) -< _0
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _3)
    _7 <- iPre 0.32281500428612553 -< _6
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _1)
    _9 <- iPre 1.1985812626265133 -< _4
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _5)
    _1 <- iPre 0.543910839537287 -< _10
  FRP.Yampa.returnA -< _8

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_0, _0}
    _3 <- arr21 (-) -< {_2, _1}
    _4 <- arr11 (+1) -< _2
    _5 <- arr11 (+1) -< _0
    _6 <- arr21 (+) -< {_2, _3}
    _7 <- pre1 0.32281500428612553 -< _6
    _8 <- arr21 (+) -< {_7, _1}
    _9 <- pre1 1.1985812626265133 -< _4
    _10 <- arr21 (+) -< {_9, _5}
    _1 <- pre1 0.543910839537287 -< _10

  GenProc.GeneralisedArrow.returnA -< _8|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10