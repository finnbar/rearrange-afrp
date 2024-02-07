{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _3 <- iPre 1.3441328831998456 -< _1
  _4 <- iPre 1.323658907887357 -< _2
  _5 <- FRP.Yampa.arr (+1) -< _4
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _5)
  _7 <- iPre 0.6818596823825385 -< _1
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _4)
  _9 <- iPre 3.0173895329550233 -< _0
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _9)
  _11 <- FRP.Yampa.arr (+1) -< _8
  _12 <- iPre 1.9983405005694708 -< _8
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _10)
  _14 <- iPre 1.4567152105106993 -< _13
  _15 <- FRP.Yampa.arr (+1) -< _3
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _15)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _16)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _14)
  _19 <- iPre 1.4869463145776647 -< _17
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _18)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (+) -< {_0, _0}
  _3 <- pre1 1.3441328831998456 -< _1
  _4 <- pre1 1.323658907887357 -< _2
  _5 <- arr11 (+1) -< _4
  _6 <- arr21 (+) -< {_3, _5}
  _7 <- pre1 0.6818596823825385 -< _1
  _8 <- arr21 (+) -< {_7, _4}
  _9 <- pre1 3.0173895329550233 -< _0
  _10 <- arr21 (*) -< {_6, _9}
  _11 <- arr11 (+1) -< _8
  _12 <- pre1 1.9983405005694708 -< _8
  _13 <- arr21 (+) -< {_5, _10}
  _14 <- pre1 1.4567152105106993 -< _13
  _15 <- arr11 (+1) -< _3
  _16 <- arr21 (-) -< {_11, _15}
  _17 <- arr21 (-) -< {_15, _16}
  _18 <- arr21 (+) -< {_12, _14}
  _19 <- pre1 1.4869463145776647 -< _17
  _20 <- arr21 (-) -< {_19, _18}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0