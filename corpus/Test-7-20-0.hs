{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.1663536217331536 -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
  _3 <- iPre 2.9670794175437476 -< _2
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _3)
  _5 <- iPre 0.9840646741375992 -< _4
  _6 <- FRP.Yampa.arr (+1) -< _4
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _5)
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- FRP.Yampa.arr (+1) -< _6
  _10 <- FRP.Yampa.arr (+1) -< _4
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _9)
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _6)
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _3)
  _14 <- FRP.Yampa.arr (+1) -< _8
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _12)
  _16 <- iPre 0.8215059789674916 -< _14
  _17 <- iPre 0.6186950214297532 -< _15
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _13)
  _19 <- iPre 1.009717371386392 -< _18
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _16)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.1663536217331536 -< _0
  _2 <- arr21 (-) -< {_0, _1}
  _3 <- pre1 2.9670794175437476 -< _2
  _4 <- arr21 (*) -< {_0, _3}
  _5 <- pre1 0.9840646741375992 -< _4
  _6 <- arr11 (+1) -< _4
  _7 <- arr21 (+) -< {_2, _5}
  _8 <- arr11 (+1) -< _7
  _9 <- arr11 (+1) -< _6
  _10 <- arr11 (+1) -< _4
  _11 <- arr21 (-) -< {_10, _9}
  _12 <- arr21 (-) -< {_1, _6}
  _13 <- arr21 (+) -< {_10, _3}
  _14 <- arr11 (+1) -< _8
  _15 <- arr21 (*) -< {_11, _12}
  _16 <- pre1 0.8215059789674916 -< _14
  _17 <- pre1 0.6186950214297532 -< _15
  _18 <- arr21 (+) -< {_17, _13}
  _19 <- pre1 1.009717371386392 -< _18
  _20 <- arr21 (+) -< {_19, _16}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0