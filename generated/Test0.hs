{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  rec
    _7 <- FRP.Yampa.arr (+1) -< _1
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _3)
    _10 <- FRP.Yampa.arr (+1) -< _4
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _6)
    _12 <- FRP.Yampa.arr (+1) -< _7
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _9)
    _4 <- iPre 2.718089766367792 -< _10
    _5 <- iPre 2.659985125896286 -< _11
    _6 <- iPre 1.7077722794061059 -< _12
  _14 <- FRP.Yampa.arr (+1) -< _13
  _15 <- FRP.Yampa.arr (+1) -< _14
  _16 <- iPre 1.4857626400888464 -< _15
  _17 <- iPre 2.9206666154273457 -< _16
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- iPre 1.8567501011266385 -< _18
  _20 <- iPre 2.596930165196826 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr11 (+1) -< _0
  _3 <- arr21 (+) -< {_1, _1}
  rec
    _7 <- arr11 (+1) -< _1
    _8 <- arr21 (-) -< {_1, _2}
    _9 <- arr21 (*) -< {_2, _3}
    _10 <- arr11 (+1) -< _4
    _11 <- arr21 (-) -< {_5, _6}
    _12 <- arr11 (+1) -< _7
    _13 <- arr21 (-) -< {_8, _9}
    _4 <- pre1 2.718089766367792 -< _10
    _5 <- pre1 2.659985125896286 -< _11
    _6 <- pre1 1.7077722794061059 -< _12

  _14 <- arr11 (+1) -< _13
  _15 <- arr11 (+1) -< _14
  _16 <- pre1 1.4857626400888464 -< _15
  _17 <- pre1 2.9206666154273457 -< _16
  _18 <- arr11 (+1) -< _17
  _19 <- pre1 1.8567501011266385 -< _18
  _20 <- pre1 2.596930165196826 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10