{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _0)
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _0)
    _1 <- iPre 2.6527028509766204 -< _4
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _3)
  _6 <- iPre 2.9633420805947113 -< _3
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- FRP.Yampa.arr (+1) -< _0
  _10 <- iPre 1.8302462621531133 -< _3
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _9)
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _2)
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _12)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _5)
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _13)
  _16 <- iPre 2.637915793679223 -< _6
  _17 <- FRP.Yampa.arr (+1) -< _16
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _14)
  _19 <- FRP.Yampa.arr (+1) -< _18
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _15)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_1, _1}
    _3 <- arr21 (-) -< {_2, _0}
    _4 <- arr21 (-) -< {_3, _0}
    _1 <- pre1 2.6527028509766204 -< _4

  _5 <- arr21 (+) -< {_2, _3}
  _6 <- pre1 2.9633420805947113 -< _3
  _7 <- arr21 (+) -< {_0, _2}
  _8 <- arr11 (+1) -< _7
  _9 <- arr11 (+1) -< _0
  _10 <- pre1 1.8302462621531133 -< _3
  _11 <- arr21 (+) -< {_1, _9}
  _12 <- arr21 (+) -< {_3, _2}
  _13 <- arr21 (-) -< {_11, _12}
  _14 <- arr21 (+) -< {_8, _5}
  _15 <- arr21 (-) -< {_10, _13}
  _16 <- pre1 2.637915793679223 -< _6
  _17 <- arr11 (+1) -< _16
  _18 <- arr21 (*) -< {_17, _14}
  _19 <- arr11 (+1) -< _18
  _20 <- arr21 (+) -< {_19, _15}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4