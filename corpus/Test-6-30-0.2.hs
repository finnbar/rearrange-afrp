{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
  rec
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _4)
    _7 <- iPre 2.392339576620034 -< _6
    _8 <- iPre 0.5403577314897269 -< _5
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _5)
    _4 <- iPre 1.1569841639128373 -< _8
    _5 <- iPre 0.7465469730136841 -< _1
  _10 <- FRP.Yampa.arr (+1) -< _1
  _11 <- iPre 2.7591997707493685 -< _0
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _10)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _11)
  _14 <- iPre 0.7733965220532919 -< _12
  _15 <- FRP.Yampa.arr (+1) -< _7
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _15)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _16)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _9)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _0)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _19)
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _6)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _21)
  _23 <- FRP.Yampa.arr (+1) -< _19
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _20)
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _24)
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _23)
  _27 <- iPre 0.14605433292735873 -< _26
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _13)
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _28)
  _30 <- iPre 1.7190008850265808 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- arr21 (*) -< {_2, _1}
  rec
    _6 <- arr21 (-) -< {_3, _4}
    _7 <- pre1 2.392339576620034 -< _6
    _8 <- pre1 0.5403577314897269 -< _5
    _9 <- arr21 (+) -< {_7, _5}
    _4 <- pre1 1.1569841639128373 -< _8
    _5 <- pre1 0.7465469730136841 -< _1

  _10 <- arr11 (+1) -< _1
  _11 <- pre1 2.7591997707493685 -< _0
  _12 <- arr21 (*) -< {_3, _10}
  _13 <- arr21 (*) -< {_1, _11}
  _14 <- pre1 0.7733965220532919 -< _12
  _15 <- arr11 (+1) -< _7
  _16 <- arr21 (-) -< {_14, _15}
  _17 <- arr21 (+) -< {_2, _16}
  _18 <- arr21 (+) -< {_11, _9}
  _19 <- arr21 (-) -< {_18, _0}
  _20 <- arr21 (-) -< {_2, _19}
  _21 <- arr21 (*) -< {_20, _6}
  _22 <- arr21 (-) -< {_20, _21}
  _23 <- arr11 (+1) -< _19
  _24 <- arr21 (+) -< {_17, _20}
  _25 <- arr21 (+) -< {_6, _24}
  _26 <- arr21 (+) -< {_22, _23}
  _27 <- pre1 0.14605433292735873 -< _26
  _28 <- arr21 (*) -< {_27, _13}
  _29 <- arr21 (*) -< {_25, _28}
  _30 <- pre1 1.7190008850265808 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6