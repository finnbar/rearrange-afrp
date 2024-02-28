{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 1.4189482690427482 -< _1
    _1 <- iPre 2.1677248482502365 -< _2
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _0)
  _4 <- FRP.Yampa.arr (+1) -< _3
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _4)
  _6 <- FRP.Yampa.arr (+1) -< _0
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _5)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _7)
  _10 <- iPre 3.5142755844889657 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 1.4189482690427482 -< _1
    _1 <- pre1 2.1677248482502365 -< _2

  _3 <- arr21 (-) -< {_2, _0}
  _4 <- arr11 (+1) -< _3
  _5 <- arr21 (-) -< {_3, _4}
  _6 <- arr11 (+1) -< _0
  _7 <- arr11 (+1) -< _6
  _8 <- arr21 (-) -< {_0, _5}
  _9 <- arr21 (*) -< {_8, _7}
  _10 <- pre1 3.5142755844889657 -< _9
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2