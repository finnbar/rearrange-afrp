{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
    _2 <- iPre 0.6329924048665043 -< _3
  _4 <- iPre 1.8152495541874663 -< _3
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _2)
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _5)
  _7 <- FRP.Yampa.arr (+1) -< _0
  _8 <- iPre 4.503362954760515 -< _7
  _9 <- iPre 2.0553210429472517 -< _8
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _6)
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  rec
    _3 <- arr21 (-) -< {_1, _2}
    _2 <- pre1 0.6329924048665043 -< _3

  _4 <- pre1 1.8152495541874663 -< _3
  _5 <- arr21 (*) -< {_4, _2}
  _6 <- arr21 (-) -< {_1, _5}
  _7 <- arr11 (+1) -< _0
  _8 <- pre1 4.503362954760515 -< _7
  _9 <- pre1 2.0553210429472517 -< _8
  _10 <- arr21 (-) -< {_9, _6}
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2