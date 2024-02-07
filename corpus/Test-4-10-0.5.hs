{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (+1) -< _1
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
    _1 <- iPre 2.629114399142835 -< _0
    _2 <- iPre 2.7760455248165155 -< _3
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _5)
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _6)
  _8 <- iPre 1.703848420393424 -< _3
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _8)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _7)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr11 (+1) -< _1
    _4 <- arr21 (+) -< {_0, _2}
    _5 <- arr21 (-) -< {_2, _1}
    _1 <- pre1 2.629114399142835 -< _0
    _2 <- pre1 2.7760455248165155 -< _3

  _6 <- arr21 (*) -< {_4, _5}
  _7 <- arr21 (*) -< {_6, _6}
  _8 <- pre1 1.703848420393424 -< _3
  _9 <- arr21 (*) -< {_0, _8}
  _10 <- arr21 (-) -< {_9, _7}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5