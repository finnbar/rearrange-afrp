{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (+1) -< _0
    _3 <- iPre 0.36953908358535104 -< _0
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
    _5 <- FRP.Yampa.arr (+1) -< _0
    _6 <- FRP.Yampa.arr (+1) -< _4
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _0)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _6)
    _9 <- iPre 2.687631158236502 -< _5
    _10 <- FRP.Yampa.arr (+1) -< _6
    _11 <- iPre 1.2393521026410876 -< _6
    _12 <- FRP.Yampa.arr (+1) -< _7
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _5)
    _14 <- iPre 1.4770745610455445 -< _1
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _3)
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _13)
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _15)
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _16)
    _19 <- iPre 2.705389476121449 -< _18
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _14)
    _1 <- iPre 1.820738219469043e-2 -< _8
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _0
    _3 <- pre1 0.36953908358535104 -< _0
    _4 <- arr21 (-) -< {_0, _2}
    _5 <- arr11 (+1) -< _0
    _6 <- arr11 (+1) -< _4
    _7 <- arr21 (*) -< {_4, _0}
    _8 <- arr21 (+) -< {_7, _6}
    _9 <- pre1 2.687631158236502 -< _5
    _10 <- arr11 (+1) -< _6
    _11 <- pre1 1.2393521026410876 -< _6
    _12 <- arr11 (+1) -< _7
    _13 <- arr21 (-) -< {_11, _5}
    _14 <- pre1 1.4770745610455445 -< _1
    _15 <- arr21 (*) -< {_9, _3}
    _16 <- arr21 (-) -< {_12, _13}
    _17 <- arr21 (+) -< {_10, _15}
    _18 <- arr21 (+) -< {_17, _16}
    _19 <- pre1 2.705389476121449 -< _18
    _20 <- arr21 (-) -< {_19, _14}
    _1 <- pre1 1.820738219469043e-2 -< _8

  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20