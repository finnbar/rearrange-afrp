{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 0.7185860079549885 -< _1
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _3)
    _1 <- iPre 2.562311385235658 -< _5
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _6)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _7)
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 0.7185860079549885 -< _1
    _3 <- arr21 (*) -< {_1, _1}
    _4 <- arr21 (*) -< {_1, _1}
    _5 <- arr21 (-) -< {_3, _3}
    _1 <- pre1 2.562311385235658 -< _5

  _6 <- arr21 (*) -< {_1, _2}
  _7 <- arr21 (*) -< {_0, _6}
  _8 <- arr21 (*) -< {_4, _7}
  _9 <- arr11 (+1) -< _8
  _10 <- arr11 (+1) -< _9
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5