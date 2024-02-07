{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.461278203818623 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (+1) -< _0
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
  _5 <- iPre 0.5567911082424007 -< _4
  _6 <- iPre 2.2714180052224213 -< _5
  _7 <- FRP.Yampa.arr (+1) -< _2
  _8 <- FRP.Yampa.arr (+1) -< _2
  _9 <- FRP.Yampa.arr (+1) -< _2
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _4)
  _11 <- iPre 1.5616732934007787 -< _7
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _10)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _4)
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _8)
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _13)
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _6)
  _17 <- iPre 2.7387017641541327 -< _16
  _18 <- iPre 0.29841477381637777 -< _17
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _12)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.461278203818623 -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- arr11 (+1) -< _0
  _4 <- arr21 (-) -< {_2, _1}
  _5 <- pre1 0.5567911082424007 -< _4
  _6 <- pre1 2.2714180052224213 -< _5
  _7 <- arr11 (+1) -< _2
  _8 <- arr11 (+1) -< _2
  _9 <- arr11 (+1) -< _2
  _10 <- arr21 (-) -< {_4, _4}
  _11 <- pre1 1.5616732934007787 -< _7
  _12 <- arr21 (*) -< {_9, _10}
  _13 <- arr21 (*) -< {_3, _4}
  _14 <- arr21 (-) -< {_1, _8}
  _15 <- arr21 (-) -< {_14, _13}
  _16 <- arr21 (-) -< {_11, _6}
  _17 <- pre1 2.7387017641541327 -< _16
  _18 <- pre1 0.29841477381637777 -< _17
  _19 <- arr21 (*) -< {_15, _12}
  _20 <- arr21 (-) -< {_18, _19}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0