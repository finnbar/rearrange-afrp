{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
  _4 <- iPre 0.42182198375332886 -< _3
  _5 <- iPre 0.6037548067619533 -< _3
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _1)
  _7 <- FRP.Yampa.arr (+1) -< _1
  _8 <- iPre 1.60444377249783 -< _4
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _6)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- arr21 (*) -< {_1, _2}
  _4 <- pre1 0.42182198375332886 -< _3
  _5 <- pre1 0.6037548067619533 -< _3
  _6 <- arr21 (*) -< {_5, _1}
  _7 <- arr11 (+1) -< _1
  _8 <- pre1 1.60444377249783 -< _4
  _9 <- arr21 (+) -< {_8, _6}
  _10 <- arr21 (*) -< {_7, _9}
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0