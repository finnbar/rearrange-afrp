{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
    _4 <- iPre 2.6891357076967086 -< _3
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _3)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _2)
    _7 <- FRP.Yampa.arr (+1) -< _6
    _8 <- iPre 2.906897178952013 -< _5
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- iPre 1.538221974907723 -< _9
    _1 <- iPre 0.7149797038241453 -< _10
  FRP.Yampa.returnA -< _7

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_0, _1}
    _3 <- arr21 (*) -< {_2, _1}
    _4 <- pre1 2.6891357076967086 -< _3
    _5 <- arr21 (-) -< {_4, _3}
    _6 <- arr21 (-) -< {_4, _2}
    _7 <- arr11 (+1) -< _6
    _8 <- pre1 2.906897178952013 -< _5
    _9 <- arr11 (+1) -< _8
    _10 <- pre1 1.538221974907723 -< _9
    _1 <- pre1 0.7149797038241453 -< _10

  AFRP.returnA -< _7|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10