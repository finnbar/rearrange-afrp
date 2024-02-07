{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _4 <- FRP.Yampa.arr (+1) -< _1
  _5 <- iPre 2.3470210915348244 -< _2
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _7)
  _9 <- FRP.Yampa.arr (+1) -< _5
  _10 <- FRP.Yampa.arr (+1) -< _2
  _11 <- FRP.Yampa.arr (+1) -< _4
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _1)
  _13 <- FRP.Yampa.arr (+1) -< _9
  _14 <- iPre 0.5230846250921322 -< _11
  _15 <- iPre 0.7245144270759568 -< _13
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _14)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _12)
  _18 <- iPre 2.190535332846091 -< _17
  _19 <- iPre 0.38379644322532536 -< _8
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _18)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (*) -< {_0, _0}
  _3 <- arr21 (+) -< {_1, _0}
  _4 <- arr11 (+1) -< _1
  _5 <- pre1 2.3470210915348244 -< _2
  _6 <- arr21 (-) -< {_2, _2}
  _7 <- arr21 (-) -< {_2, _3}
  _8 <- arr21 (-) -< {_6, _7}
  _9 <- arr11 (+1) -< _5
  _10 <- arr11 (+1) -< _2
  _11 <- arr11 (+1) -< _4
  _12 <- arr21 (+) -< {_10, _1}
  _13 <- arr11 (+1) -< _9
  _14 <- pre1 0.5230846250921322 -< _11
  _15 <- pre1 0.7245144270759568 -< _13
  _16 <- arr21 (*) -< {_15, _14}
  _17 <- arr21 (-) -< {_16, _12}
  _18 <- pre1 2.190535332846091 -< _17
  _19 <- pre1 0.38379644322532536 -< _8
  _20 <- arr21 (*) -< {_19, _18}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0