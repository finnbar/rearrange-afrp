{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
    _4 <- iPre 2.379678257775347 -< _0
    _5 <- iPre 1.2900196412630258 -< _1
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _5)
    _8 <- FRP.Yampa.arr (+1) -< _3
    _9 <- iPre 1.9313317464962994 -< _4
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _7)
    _1 <- iPre 3.03121890122202 -< _10
    _2 <- iPre 1.1585562151669262 -< _8
  FRP.Yampa.returnA -< _6

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr21 (+) -< {_0, _2}
    _4 <- pre1 2.379678257775347 -< _0
    _5 <- pre1 1.2900196412630258 -< _1
    _6 <- arr21 (*) -< {_0, _1}
    _7 <- arr21 (-) -< {_1, _5}
    _8 <- arr11 (+1) -< _3
    _9 <- pre1 1.9313317464962994 -< _4
    _10 <- arr21 (+) -< {_9, _7}
    _1 <- pre1 3.03121890122202 -< _10
    _2 <- pre1 1.1585562151669262 -< _8

  GenProc.GeneralisedArrow.returnA -< _6|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10