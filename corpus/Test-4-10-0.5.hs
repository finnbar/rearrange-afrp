{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (+1) -< _1
    _3 <- iPre 1.8244780908655402 -< _0
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _1)
    _5 <- iPre 0.6210241335197109 -< _1
    _1 <- iPre 1.1604595830320208 -< _3
  _6 <- iPre 2.6233885900671643 -< _1
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _6)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _7)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _4)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _1
    _3 <- pre1 1.8244780908655402 -< _0
    _4 <- arr21 (+) -< {_3, _1}
    _5 <- pre1 0.6210241335197109 -< _1
    _1 <- pre1 1.1604595830320208 -< _3

  _6 <- pre1 2.6233885900671643 -< _1
  _7 <- arr21 (*) -< {_3, _6}
  _8 <- arr21 (*) -< {_2, _7}
  _9 <- arr21 (+) -< {_8, _4}
  _10 <- arr21 (+) -< {_5, _9}
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5