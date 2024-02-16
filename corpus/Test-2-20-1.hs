{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _3 <- FRP.Yampa.arr (+1) -< _1
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _5)
    _7 <- iPre 2.658351692637319 -< _6
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _0)
    _9 <- FRP.Yampa.arr (+1) -< _4
    _10 <- FRP.Yampa.arr (+1) -< _6
    _11 <- FRP.Yampa.arr (+1) -< _8
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _6)
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _10)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _3)
    _15 <- FRP.Yampa.arr (+1) -< _12
    _16 <- iPre 1.2890484445250883 -< _14
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _13)
    _18 <- FRP.Yampa.arr (+1) -< _16
    _19 <- iPre 0.6060193479601792 -< _18
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _17)
    _1 <- iPre 1.549610918834719 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_1, _1}
    _3 <- arr11 (+1) -< _1
    _4 <- arr21 (-) -< {_1, _2}
    _5 <- arr21 (+) -< {_0, _0}
    _6 <- arr21 (-) -< {_1, _5}
    _7 <- pre1 2.658351692637319 -< _6
    _8 <- arr21 (+) -< {_6, _0}
    _9 <- arr11 (+1) -< _4
    _10 <- arr11 (+1) -< _6
    _11 <- arr11 (+1) -< _8
    _12 <- arr21 (+) -< {_8, _6}
    _13 <- arr21 (+) -< {_7, _10}
    _14 <- arr21 (+) -< {_9, _3}
    _15 <- arr11 (+1) -< _12
    _16 <- pre1 1.2890484445250883 -< _14
    _17 <- arr21 (+) -< {_15, _13}
    _18 <- arr11 (+1) -< _16
    _19 <- pre1 0.6060193479601792 -< _18
    _20 <- arr21 (+) -< {_11, _17}
    _1 <- pre1 1.549610918834719 -< _19

  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20