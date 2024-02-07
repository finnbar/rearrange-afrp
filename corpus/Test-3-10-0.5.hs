{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  rec
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _1)
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _2)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _3)
    _2 <- iPre 2.443538071565506 -< _6
    _3 <- iPre 2.228651329347952 -< _6
  _7 <- FRP.Yampa.arr (+1) -< _1
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _1)
  _9 <- iPre 2.3433800021719717 -< _8
  _10 <- iPre 2.048452131681202 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  rec
    _4 <- arr21 (*) -< {_3, _1}
    _5 <- arr21 (*) -< {_4, _2}
    _6 <- arr21 (*) -< {_5, _3}
    _2 <- pre1 2.443538071565506 -< _6
    _3 <- pre1 2.228651329347952 -< _6

  _7 <- arr11 (+1) -< _1
  _8 <- arr21 (*) -< {_7, _1}
  _9 <- pre1 2.3433800021719717 -< _8
  _10 <- pre1 2.048452131681202 -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5