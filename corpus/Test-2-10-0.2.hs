{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _1 <- iPre 3.129791332388514 -< _2
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
  _4 <- FRP.Yampa.arr (+1) -< _1
  _5 <- iPre 2.4372458683914924 -< _2
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _5)
  _7 <- iPre 3.0078449839229586 -< _4
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _8)
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (+) -< {_0, _1}
    _1 <- pre1 3.129791332388514 -< _2

  _3 <- arr21 (+) -< {_1, _2}
  _4 <- arr11 (+1) -< _1
  _5 <- pre1 2.4372458683914924 -< _2
  _6 <- arr21 (+) -< {_3, _5}
  _7 <- pre1 3.0078449839229586 -< _4
  _8 <- arr11 (+1) -< _7
  _9 <- arr21 (-) -< {_6, _8}
  _10 <- arr11 (+1) -< _9
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2