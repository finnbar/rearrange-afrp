{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  rec
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _5)
    _9 <- iPre 2.7340914148366626 -< _1
    _10 <- iPre 2.868046606364371 -< _5
    _11 <- iPre 0.7309846314116519 -< _4
    _12 <- iPre 0.11550271107756414 -< _6
    _13 <- FRP.Yampa.arr (+1) -< _12
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _9)
    _15 <- iPre 1.3361448478923914 -< _11
    _16 <- iPre 1.623939903913093 -< _0
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _14)
    _3 <- iPre 1.8257363228398795 -< _0
    _4 <- iPre 2.5420212872692693 -< _14
    _5 <- iPre 3.968598583573506e-2 -< _5
    _6 <- iPre 1.4057896546528545 -< _14
    _7 <- iPre 2.8495187024236026 -< _15
  _18 <- FRP.Yampa.arr (+1) -< _13
  _19 <- iPre 0.15197870932945579 -< _17
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _8)
  _21 <- FRP.Yampa.arr (+1) -< _20
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _7)
  _23 <- iPre 8.159076582492944e-2 -< _3
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _16)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _24)
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _21)
  _27 <- iPre 1.472051710281638 -< _25
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _26)
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _28)
  _30 <- iPre 2.64563692134546 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _0}
  rec
    _8 <- arr21 (*) -< {_5, _5}
    _9 <- pre1 2.7340914148366626 -< _1
    _10 <- pre1 2.868046606364371 -< _5
    _11 <- pre1 0.7309846314116519 -< _4
    _12 <- pre1 0.11550271107756414 -< _6
    _13 <- arr11 (+1) -< _12
    _14 <- arr21 (+) -< {_5, _9}
    _15 <- pre1 1.3361448478923914 -< _11
    _16 <- pre1 1.623939903913093 -< _0
    _17 <- arr21 (*) -< {_1, _14}
    _3 <- pre1 1.8257363228398795 -< _0
    _4 <- pre1 2.5420212872692693 -< _14
    _5 <- pre1 3.968598583573506e-2 -< _5
    _6 <- pre1 1.4057896546528545 -< _14
    _7 <- pre1 2.8495187024236026 -< _15

  _18 <- arr11 (+1) -< _13
  _19 <- pre1 0.15197870932945579 -< _17
  _20 <- arr21 (+) -< {_2, _8}
  _21 <- arr11 (+1) -< _20
  _22 <- arr21 (*) -< {_18, _7}
  _23 <- pre1 8.159076582492944e-2 -< _3
  _24 <- arr21 (+) -< {_23, _16}
  _25 <- arr21 (-) -< {_22, _24}
  _26 <- arr21 (-) -< {_19, _21}
  _27 <- pre1 1.472051710281638 -< _25
  _28 <- arr21 (+) -< {_27, _26}
  _29 <- arr21 (-) -< {_10, _28}
  _30 <- pre1 2.64563692134546 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15