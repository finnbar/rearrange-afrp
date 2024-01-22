{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  rec
    _5 <- FRP.Yampa.arr (+1) -< _0
    _6 <- iPre 2.8379532659832343 -< _0
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
    _8 <- iPre 2.638193276613887 -< _2
    _9 <- FRP.Yampa.arr (+1) -< _3
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _3)
    _11 <- FRP.Yampa.arr (+1) -< _4
    _2 <- iPre 1.4641640158576155 -< _5
    _3 <- iPre 2.748023088088167 -< _4
    _4 <- iPre 0.6072887765950603 -< _6
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _6)
  _13 <- FRP.Yampa.arr (+1) -< _7
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _9)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _11)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _13)
  _17 <- iPre 1.098595927926575 -< _14
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _16)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _18)
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  rec
    _5 <- arr11 (+1) -< _0
    _6 <- pre1 2.8379532659832343 -< _0
    _7 <- arr21 (*) -< {_0, _1}
    _8 <- pre1 2.638193276613887 -< _2
    _9 <- arr11 (+1) -< _3
    _10 <- arr21 (*) -< {_2, _3}
    _11 <- arr11 (+1) -< _4
    _2 <- pre1 1.4641640158576155 -< _5
    _3 <- pre1 2.748023088088167 -< _4
    _4 <- pre1 0.6072887765950603 -< _6

  _12 <- arr21 (+) -< {_1, _6}
  _13 <- arr11 (+1) -< _7
  _14 <- arr21 (+) -< {_8, _9}
  _15 <- arr21 (+) -< {_10, _11}
  _16 <- arr21 (+) -< {_12, _13}
  _17 <- pre1 1.098595927926575 -< _14
  _18 <- arr21 (*) -< {_15, _16}
  _19 <- arr21 (-) -< {_17, _18}
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10