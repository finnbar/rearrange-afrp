{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
    _2 <- iPre 3.238592653735245 -< _3
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _4)
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _5)
  _7 <- iPre 8.12206529329395e-3 -< _3
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _5)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _7)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  rec
    _3 <- arr21 (-) -< {_0, _2}
    _2 <- pre1 3.238592653735245 -< _3

  _4 <- arr21 (-) -< {_1, _3}
  _5 <- arr21 (-) -< {_0, _4}
  _6 <- arr21 (*) -< {_5, _5}
  _7 <- pre1 8.12206529329395e-3 -< _3
  _8 <- arr21 (*) -< {_6, _5}
  _9 <- arr21 (*) -< {_8, _7}
  _10 <- arr21 (-) -< {_6, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2