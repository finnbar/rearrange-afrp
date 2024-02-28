{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
    _3 <- FRP.Yampa.arr (+1) -< _2
    _4 <- FRP.Yampa.arr (+1) -< _2
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _3)
    _6 <- iPre 1.3528451967483643 -< _2
    _7 <- FRP.Yampa.arr (+1) -< _6
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _7)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _8)
    _10 <- FRP.Yampa.arr (+1) -< _9
    _1 <- iPre 0.5521932772613077 -< _10
  FRP.Yampa.returnA -< _5

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_0, _1}
    _3 <- arr11 (+1) -< _2
    _4 <- arr11 (+1) -< _2
    _5 <- arr21 (*) -< {_1, _3}
    _6 <- pre1 1.3528451967483643 -< _2
    _7 <- arr11 (+1) -< _6
    _8 <- arr21 (*) -< {_7, _7}
    _9 <- arr21 (-) -< {_4, _8}
    _10 <- arr11 (+1) -< _9
    _1 <- pre1 0.5521932772613077 -< _10

  AFRP.returnA -< _5|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10