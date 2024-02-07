{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _7 <- iPre 0.3579361759318107 -< _5
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _5)
    _9 <- iPre 2.2290341457713576 -< _5
    _10 <- iPre 2.5771757354796105 -< _4
    _11 <- FRP.Yampa.arr (+1) -< _3
    _12 <- FRP.Yampa.arr (+1) -< _8
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _1)
    _14 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _9)
    _15 <- iPre 1.9605772308931602 -< _2
    _16 <- iPre 2.669405044212987 -< _11
    _17 <- iPre 1.8285081879947611 -< _14
    _18 <- iPre 2.324573907653267 -< _0
    _19 <- FRP.Yampa.arr (+1) -< _16
    _20 <- iPre 1.6133325084450325 -< _15
    _1 <- iPre 2.9912017087648235 -< _6
    _2 <- iPre 0.9213380642828612 -< _18
    _3 <- iPre 0.3565916420602912 -< _20
    _4 <- iPre 1.547943906882934 -< _7
    _5 <- iPre 0.22096953205398237 -< _13
    _6 <- iPre 1.1139625068047432 -< _19
  FRP.Yampa.returnA -< _17

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _7 <- pre1 0.3579361759318107 -< _5
    _8 <- arr21 (*) -< {_5, _5}
    _9 <- pre1 2.2290341457713576 -< _5
    _10 <- pre1 2.5771757354796105 -< _4
    _11 <- arr11 (+1) -< _3
    _12 <- arr11 (+1) -< _8
    _13 <- arr21 (+) -< {_10, _1}
    _14 <- arr21 (-) -< {_12, _9}
    _15 <- pre1 1.9605772308931602 -< _2
    _16 <- pre1 2.669405044212987 -< _11
    _17 <- pre1 1.8285081879947611 -< _14
    _18 <- pre1 2.324573907653267 -< _0
    _19 <- arr11 (+1) -< _16
    _20 <- pre1 1.6133325084450325 -< _15
    _1 <- pre1 2.9912017087648235 -< _6
    _2 <- pre1 0.9213380642828612 -< _18
    _3 <- pre1 0.3565916420602912 -< _20
    _4 <- pre1 1.547943906882934 -< _7
    _5 <- pre1 0.22096953205398237 -< _13
    _6 <- pre1 1.1139625068047432 -< _19

  GenProc.GeneralisedArrow.returnA -< _17|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20