{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
  _4 <- iPre 2.8147810582019512 -< _3
  _5 <- FRP.Yampa.arr (+1) -< _4
  _6 <- iPre 1.612192978547046 -< _5
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _6)
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- iPre 2.698421353248674 -< _8
  _10 <- iPre 1.0150190320559513 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (-) -< {_0, _0}
  _3 <- arr21 (-) -< {_0, _1}
  _4 <- pre1 2.8147810582019512 -< _3
  _5 <- arr11 (+1) -< _4
  _6 <- pre1 1.612192978547046 -< _5
  _7 <- arr21 (-) -< {_2, _6}
  _8 <- arr11 (+1) -< _7
  _9 <- pre1 2.698421353248674 -< _8
  _10 <- pre1 1.0150190320559513 -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0