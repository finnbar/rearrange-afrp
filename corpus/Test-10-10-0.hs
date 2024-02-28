{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  _3 <- iPre 1.5239251699196212 -< _2
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _5 <- FRP.Yampa.arr (+1) -< _3
  _6 <- iPre 2.6779040725180874 -< _5
  _7 <- iPre 2.1240513427710197 -< _4
  _8 <- iPre 1.8961926690077557 -< _7
  _9 <- FRP.Yampa.arr (+1) -< _6
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _0}
  _3 <- pre1 1.5239251699196212 -< _2
  _4 <- arr21 (*) -< {_1, _1}
  _5 <- arr11 (+1) -< _3
  _6 <- pre1 2.6779040725180874 -< _5
  _7 <- pre1 2.1240513427710197 -< _4
  _8 <- pre1 1.8961926690077557 -< _7
  _9 <- arr11 (+1) -< _6
  _10 <- arr21 (*) -< {_8, _9}
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0