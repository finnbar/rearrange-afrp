{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  rec
    _3 <- FRP.Yampa.arr (+1) -< _2
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _2)
    _2 <- iPre 1.4766778856955836 -< _1
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _7)
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _5)
  _10 <- iPre 1.7696982490678432 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  rec
    _3 <- arr11 (+1) -< _2
    _4 <- arr21 (*) -< {_1, _0}
    _5 <- arr21 (*) -< {_1, _1}
    _6 <- arr21 (-) -< {_4, _2}
    _2 <- pre1 1.4766778856955836 -< _1

  _7 <- arr11 (+1) -< _6
  _8 <- arr21 (*) -< {_3, _7}
  _9 <- arr21 (-) -< {_8, _5}
  _10 <- pre1 1.7696982490678432 -< _9
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5