{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 35.03287621604911 -< _0
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
    _2 <- iPre 14.61555770320927 -< _3
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _0)
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _1)
  _6 <- FRP.Yampa.arr (+1) -< _5
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _0)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _2)
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 35.03287621604911 -< _0
  rec
    _3 <- arr21 (-) -< {_1, _2}
    _2 <- pre1 14.61555770320927 -< _3

  _4 <- arr21 (*) -< {_2, _0}
  _5 <- arr21 (-) -< {_4, _1}
  _6 <- arr11 (+1) -< _5
  _7 <- arr21 (*) -< {_6, _0}
  _8 <- arr21 (*) -< {_7, _2}
  _9 <- arr11 (+1) -< _8
  _10 <- arr21 (+) -< {_5, _9}
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2