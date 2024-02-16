{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- iPre 2.8100161995814736 -< _1
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _3)
  _5 <- iPre 1.3108691418787306 -< _2
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
  _7 <- FRP.Yampa.arr (+1) -< _3
  _8 <- iPre 2.262385286892487 -< _5
  _9 <- iPre 0.7095628368123635 -< _6
  _10 <- FRP.Yampa.arr (+1) -< _9
  _11 <- FRP.Yampa.arr (+1) -< _8
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _4)
  _13 <- iPre 1.4041918194775185 -< _12
  _14 <- FRP.Yampa.arr (+1) -< _10
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _7)
  _16 <- iPre 1.544601202250621 -< _14
  _17 <- iPre 2.0900113758022774 -< _15
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _16)
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- pre1 2.8100161995814736 -< _1
  _3 <- arr21 (-) -< {_1, _2}
  _4 <- arr21 (*) -< {_3, _3}
  _5 <- pre1 1.3108691418787306 -< _2
  _6 <- arr21 (-) -< {_0, _2}
  _7 <- arr11 (+1) -< _3
  _8 <- pre1 2.262385286892487 -< _5
  _9 <- pre1 0.7095628368123635 -< _6
  _10 <- arr11 (+1) -< _9
  _11 <- arr11 (+1) -< _8
  _12 <- arr21 (-) -< {_11, _4}
  _13 <- pre1 1.4041918194775185 -< _12
  _14 <- arr11 (+1) -< _10
  _15 <- arr21 (+) -< {_13, _7}
  _16 <- pre1 1.544601202250621 -< _14
  _17 <- pre1 2.0900113758022774 -< _15
  _18 <- arr11 (+1) -< _17
  _19 <- arr21 (-) -< {_18, _16}
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0