{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (+1) -< _2
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _3)
    _1 <- iPre 0.6831508845772211 -< _5
  _6 <- iPre 0.49951922580475044 -< _4
  _7 <- iPre 1.9846813117819384 -< _6
  _8 <- iPre 2.9223155507329253 -< _7
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- iPre 1.364526343496103 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (+) -< {_0, _1}
    _3 <- arr21 (*) -< {_1, _2}
    _4 <- arr11 (+1) -< _2
    _5 <- arr21 (+) -< {_1, _3}
    _1 <- pre1 0.6831508845772211 -< _5

  _6 <- pre1 0.49951922580475044 -< _4
  _7 <- pre1 1.9846813117819384 -< _6
  _8 <- pre1 2.9223155507329253 -< _7
  _9 <- arr11 (+1) -< _8
  _10 <- pre1 1.364526343496103 -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5