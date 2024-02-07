{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (+1) -< _0
    _1 <- iPre 7.374978876723037e-2 -< _3
    _2 <- iPre 2.9819940435709995 -< _3
  _5 <- iPre 2.1575253102255236 -< _1
  _6 <- FRP.Yampa.arr (+1) -< _5
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _9)
  _11 <- iPre 3.002492072237788 -< _0
  _12 <- FRP.Yampa.arr (+1) -< _11
  _13 <- FRP.Yampa.arr (+1) -< _2
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _10)
  _15 <- iPre 3.090583509065386 -< _13
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _12)
  _17 <- iPre 1.674641701905244 -< _16
  _18 <- FRP.Yampa.arr (+1) -< _15
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _18)
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr21 (-) -< {_1, _2}
    _4 <- arr11 (+1) -< _0
    _1 <- pre1 7.374978876723037e-2 -< _3
    _2 <- pre1 2.9819940435709995 -< _3

  _5 <- pre1 2.1575253102255236 -< _1
  _6 <- arr11 (+1) -< _5
  _7 <- arr11 (+1) -< _6
  _8 <- arr11 (+1) -< _7
  _9 <- arr11 (+1) -< _8
  _10 <- arr21 (-) -< {_4, _9}
  _11 <- pre1 3.002492072237788 -< _0
  _12 <- arr11 (+1) -< _11
  _13 <- arr11 (+1) -< _2
  _14 <- arr21 (*) -< {_5, _10}
  _15 <- pre1 3.090583509065386 -< _13
  _16 <- arr21 (-) -< {_14, _12}
  _17 <- pre1 1.674641701905244 -< _16
  _18 <- arr11 (+1) -< _15
  _19 <- arr21 (-) -< {_17, _18}
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4