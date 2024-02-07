{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (+1) -< _1
  _4 <- FRP.Yampa.arr (+1) -< _3
  rec
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _5)
    _8 <- FRP.Yampa.arr (+1) -< _7
    _5 <- iPre 3.2240245973530968 -< _2
    _6 <- iPre 3.5714657700524937 -< _3
  _9 <- FRP.Yampa.arr (+1) -< _1
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _1)
  _11 <- iPre 3.019295094376754 -< _9
  _12 <- iPre 3.8416348760876375 -< _4
  _13 <- iPre 1.730377563979312 -< _12
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _11)
  _15 <- iPre 3.7933394973569934 -< _8
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _13)
  _18 <- FRP.Yampa.arr (+1) -< _16
  _19 <- iPre 0.11313629963360615 -< _17
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _18)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _0}
  _3 <- arr11 (+1) -< _1
  _4 <- arr11 (+1) -< _3
  rec
    _7 <- arr21 (*) -< {_6, _5}
    _8 <- arr11 (+1) -< _7
    _5 <- pre1 3.2240245973530968 -< _2
    _6 <- pre1 3.5714657700524937 -< _3

  _9 <- arr11 (+1) -< _1
  _10 <- arr21 (+) -< {_2, _1}
  _11 <- pre1 3.019295094376754 -< _9
  _12 <- pre1 3.8416348760876375 -< _4
  _13 <- pre1 1.730377563979312 -< _12
  _14 <- arr21 (*) -< {_10, _11}
  _15 <- pre1 3.7933394973569934 -< _8
  _16 <- arr11 (+1) -< _14
  _17 <- arr21 (*) -< {_15, _13}
  _18 <- arr11 (+1) -< _16
  _19 <- pre1 0.11313629963360615 -< _17
  _20 <- arr21 (*) -< {_19, _18}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4