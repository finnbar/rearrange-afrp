{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _3 <- FRP.Yampa.arr (+1) -< _0
    _4 <- iPre 0.5675208677556205 -< _3
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _3)
    _6 <- iPre 2.412441019494074 -< _5
    _7 <- iPre 0.2054972646240917 -< _6
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _2)
    _10 <- iPre 2.4396618534236625 -< _9
    _1 <- iPre 6.28688579603024e-2 -< _10
  FRP.Yampa.returnA -< _8

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_0, _0}
    _3 <- arr11 (+1) -< _0
    _4 <- pre1 0.5675208677556205 -< _3
    _5 <- arr21 (-) -< {_3, _3}
    _6 <- pre1 2.412441019494074 -< _5
    _7 <- pre1 0.2054972646240917 -< _6
    _8 <- arr21 (+) -< {_4, _1}
    _9 <- arr21 (+) -< {_7, _2}
    _10 <- pre1 2.4396618534236625 -< _9
    _1 <- pre1 6.28688579603024e-2 -< _10

  GenProc.GeneralisedArrow.returnA -< _8|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10