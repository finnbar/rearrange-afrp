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
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _3)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _4)
    _8 <- FRP.Yampa.arr (+1) -< _3
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _7)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _9)
    _11 <- iPre 0.6345619089414812 -< _8
    _2 <- iPre 2.0755616778801613 -< _10
    _3 <- iPre 1.9002615456840006 -< _8
    _4 <- iPre 1.131638727274149 -< _1
    _5 <- iPre 1.9817009304036586 -< _11
  _12 <- FRP.Yampa.arr (+1) -< _2
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _3)
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _12)
  _15 <- FRP.Yampa.arr (+1) -< _14
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _4)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _15)
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- FRP.Yampa.arr (+1) -< _18
  _20 <- iPre 2.2804932135805154 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  rec
    _6 <- arr21 (+) -< {_1, _3}
    _7 <- arr21 (*) -< {_6, _4}
    _8 <- arr11 (+1) -< _3
    _9 <- arr21 (-) -< {_5, _7}
    _10 <- arr21 (*) -< {_5, _9}
    _11 <- pre1 0.6345619089414812 -< _8
    _2 <- pre1 2.0755616778801613 -< _10
    _3 <- pre1 1.9002615456840006 -< _8
    _4 <- pre1 1.131638727274149 -< _1
    _5 <- pre1 1.9817009304036586 -< _11

  _12 <- arr11 (+1) -< _2
  _13 <- arr21 (-) -< {_12, _3}
  _14 <- arr21 (-) -< {_13, _12}
  _15 <- arr11 (+1) -< _14
  _16 <- arr21 (*) -< {_15, _4}
  _17 <- arr21 (+) -< {_16, _15}
  _18 <- arr11 (+1) -< _17
  _19 <- arr11 (+1) -< _18
  _20 <- pre1 2.2804932135805154 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10