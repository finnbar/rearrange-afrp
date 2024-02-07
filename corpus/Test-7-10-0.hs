{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.3618850076401412 -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _0)
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _4)
  _6 <- FRP.Yampa.arr (+1) -< _3
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _6)
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _7)
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.3618850076401412 -< _0
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- arr21 (-) -< {_0, _0}
  _4 <- arr21 (*) -< {_2, _0}
  _5 <- arr21 (+) -< {_3, _4}
  _6 <- arr11 (+1) -< _3
  _7 <- arr21 (+) -< {_0, _6}
  _8 <- arr21 (+) -< {_5, _7}
  _9 <- arr11 (+1) -< _8
  _10 <- arr11 (+1) -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0