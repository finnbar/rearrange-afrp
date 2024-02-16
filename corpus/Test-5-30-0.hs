{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.1326074931921863 -< _0
  _2 <- iPre 2.1637960640015 -< _1
  _3 <- iPre 0.940872915215523 -< _2
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _1)
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _5)
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _4)
  _8 <- iPre 2.0134556077343717 -< _3
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
  _10 <- FRP.Yampa.arr (+1) -< _8
  _11 <- iPre 3.024104706978366 -< _10
  _12 <- FRP.Yampa.arr (+1) -< _9
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _1)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _8)
  _15 <- FRP.Yampa.arr (+1) -< _5
  _16 <- iPre 1.351143309592602 -< _10
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _16)
  _18 <- FRP.Yampa.arr (+1) -< _14
  _19 <- iPre 0.30459919346801123 -< _12
  _20 <- iPre 2.0843558031988683 -< _17
  _21 <- iPre 1.567355998481649 -< _13
  _22 <- FRP.Yampa.arr (+1) -< _20
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _7)
  _24 <- iPre 2.179450787823604 -< _21
  _25 <- FRP.Yampa.arr (+1) -< _24
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _18)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _22)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _26)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 1.2925009825126221 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.1326074931921863 -< _0
  _2 <- pre1 2.1637960640015 -< _1
  _3 <- pre1 0.940872915215523 -< _2
  _4 <- arr21 (*) -< {_3, _2}
  _5 <- arr21 (*) -< {_3, _1}
  _6 <- arr21 (+) -< {_5, _5}
  _7 <- arr21 (+) -< {_1, _4}
  _8 <- pre1 2.0134556077343717 -< _3
  _9 <- arr21 (*) -< {_2, _1}
  _10 <- arr11 (+1) -< _8
  _11 <- pre1 3.024104706978366 -< _10
  _12 <- arr11 (+1) -< _9
  _13 <- arr21 (*) -< {_11, _1}
  _14 <- arr21 (+) -< {_6, _8}
  _15 <- arr11 (+1) -< _5
  _16 <- pre1 1.351143309592602 -< _10
  _17 <- arr21 (*) -< {_15, _16}
  _18 <- arr11 (+1) -< _14
  _19 <- pre1 0.30459919346801123 -< _12
  _20 <- pre1 2.0843558031988683 -< _17
  _21 <- pre1 1.567355998481649 -< _13
  _22 <- arr11 (+1) -< _20
  _23 <- arr21 (+) -< {_19, _7}
  _24 <- pre1 2.179450787823604 -< _21
  _25 <- arr11 (+1) -< _24
  _26 <- arr21 (*) -< {_23, _18}
  _27 <- arr21 (+) -< {_25, _22}
  _28 <- arr21 (*) -< {_27, _26}
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 1.2925009825126221 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0