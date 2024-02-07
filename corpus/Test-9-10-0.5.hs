{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
    _5 <- iPre 3.154072298976541 -< _2
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _0)
    _2 <- iPre 2.069294656700993 -< _1
    _3 <- iPre 0.4991719383044741 -< _2
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _5)
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _2)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _8)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  rec
    _4 <- arr21 (-) -< {_1, _0}
    _5 <- pre1 3.154072298976541 -< _2
    _6 <- arr21 (-) -< {_4, _0}
    _2 <- pre1 2.069294656700993 -< _1
    _3 <- pre1 0.4991719383044741 -< _2

  _7 <- arr21 (*) -< {_5, _5}
  _8 <- arr21 (-) -< {_7, _2}
  _9 <- arr21 (*) -< {_6, _8}
  _10 <- arr21 (-) -< {_3, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5