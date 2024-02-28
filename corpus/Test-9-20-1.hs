{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- FRP.Yampa.arr (+1) -< _2
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _1)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
    _8 <- iPre 0.6368469098044447 -< _0
    _9 <- FRP.Yampa.arr (+1) -< _7
    _10 <- iPre 2.5906733795141683 -< _0
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _6)
    _12 <- iPre 0.7243290418234469 -< _10
    _13 <- iPre 2.3517244472792402 -< _8
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _12)
    _15 <- iPre 2.9341723556633768 -< _13
    _16 <- iPre 6.783803480692698e-2 -< _14
    _17 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _15)
    _18 <- iPre 2.3911395392491346 -< _4
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _11)
    _20 <- FRP.Yampa.arr (+1) -< _17
    _1 <- iPre 1.713735151393222 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_1, _0}
    _3 <- arr21 (*) -< {_1, _1}
    _4 <- arr11 (+1) -< _1
    _5 <- arr11 (+1) -< _2
    _6 <- arr21 (*) -< {_3, _1}
    _7 <- arr21 (-) -< {_2, _2}
    _8 <- pre1 0.6368469098044447 -< _0
    _9 <- arr11 (+1) -< _7
    _10 <- pre1 2.5906733795141683 -< _0
    _11 <- arr21 (-) -< {_9, _6}
    _12 <- pre1 0.7243290418234469 -< _10
    _13 <- pre1 2.3517244472792402 -< _8
    _14 <- arr21 (+) -< {_5, _12}
    _15 <- pre1 2.9341723556633768 -< _13
    _16 <- pre1 6.783803480692698e-2 -< _14
    _17 <- arr21 (-) -< {_16, _15}
    _18 <- pre1 2.3911395392491346 -< _4
    _19 <- arr21 (-) -< {_18, _11}
    _20 <- arr11 (+1) -< _17
    _1 <- pre1 1.713735151393222 -< _19

  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20