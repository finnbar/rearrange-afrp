{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (+1) -< _0
    _4 <- iPre 1.7018451365363534 -< _1
    _5 <- FRP.Yampa.arr (+1) -< _4
    _6 <- FRP.Yampa.arr (+1) -< _0
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _5)
    _8 <- FRP.Yampa.arr (+1) -< _4
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _8)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _9)
    _1 <- iPre 1.0979561541693874 -< _6
    _2 <- iPre 2.988326211632512 -< _10
  FRP.Yampa.returnA -< _7

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr11 (+1) -< _0
    _4 <- pre1 1.7018451365363534 -< _1
    _5 <- arr11 (+1) -< _4
    _6 <- arr11 (+1) -< _0
    _7 <- arr21 (+) -< {_5, _5}
    _8 <- arr11 (+1) -< _4
    _9 <- arr21 (-) -< {_2, _8}
    _10 <- arr21 (*) -< {_3, _9}
    _1 <- pre1 1.0979561541693874 -< _6
    _2 <- pre1 2.988326211632512 -< _10

  GenProc.GeneralisedArrow.returnA -< _7|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10