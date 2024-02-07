{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  _3 <- iPre 2.6937820768680063 -< _2
  rec
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _5)
    _7 <- FRP.Yampa.arr (+1) -< _6
    _8 <- iPre 0.8608961503177301 -< _4
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _7)
    _10 <- iPre 0.14425097738728443 -< _3
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _2)
    _12 <- iPre 1.1925388267000086e-2 -< _5
    _13 <- iPre 1.4029016152103264 -< _11
    _4 <- iPre 1.53159864411148 -< _10
    _5 <- iPre 0.31963309552280744 -< _12
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _6)
  _15 <- FRP.Yampa.arr (+1) -< _13
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- iPre 2.460748628823813 -< _8
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _15)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _18)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _17)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr21 (+) -< {_1, _1}
  _3 <- pre1 2.6937820768680063 -< _2
  rec
    _6 <- arr21 (-) -< {_5, _5}
    _7 <- arr11 (+1) -< _6
    _8 <- pre1 0.8608961503177301 -< _4
    _9 <- arr21 (*) -< {_2, _7}
    _10 <- pre1 0.14425097738728443 -< _3
    _11 <- arr21 (+) -< {_7, _2}
    _12 <- pre1 1.1925388267000086e-2 -< _5
    _13 <- pre1 1.4029016152103264 -< _11
    _4 <- pre1 1.53159864411148 -< _10
    _5 <- pre1 0.31963309552280744 -< _12

  _14 <- arr21 (*) -< {_0, _6}
  _15 <- arr11 (+1) -< _13
  _16 <- arr11 (+1) -< _14
  _17 <- pre1 2.460748628823813 -< _8
  _18 <- arr21 (-) -< {_9, _15}
  _19 <- arr21 (+) -< {_16, _18}
  _20 <- arr21 (-) -< {_19, _17}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10