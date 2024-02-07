{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.7750505611412505 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- FRP.Yampa.arr (+1) -< _2
  rec
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _6)
    _8 <- FRP.Yampa.arr (+1) -< _0
    _9 <- FRP.Yampa.arr (+1) -< _5
    _4 <- iPre 2.0966591748994183 -< _5
    _5 <- iPre 6.557541435982116e-2 -< _8
    _6 <- iPre 1.106228893736158 -< _7
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _7)
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _9)
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _11)
  _13 <- FRP.Yampa.arr (+1) -< _9
  _14 <- FRP.Yampa.arr (+1) -< _10
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _13)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _9)
  _17 <- iPre 0.398379359242986 -< _3
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _14)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _4)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _17)
  _21 <- FRP.Yampa.arr (+1) -< _19
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _4)
  _23 <- FRP.Yampa.arr (+1) -< _0
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _15)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _22)
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _24)
  _27 <- iPre 2.3708077030536274 -< _26
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _20)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 0.31772659917403234 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.7750505611412505 -< _0
  _2 <- arr11 (+1) -< _1
  _3 <- arr11 (+1) -< _2
  rec
    _7 <- arr21 (+) -< {_3, _6}
    _8 <- arr11 (+1) -< _0
    _9 <- arr11 (+1) -< _5
    _4 <- pre1 2.0966591748994183 -< _5
    _5 <- pre1 6.557541435982116e-2 -< _8
    _6 <- pre1 1.106228893736158 -< _7

  _10 <- arr21 (*) -< {_8, _7}
  _11 <- arr21 (*) -< {_10, _9}
  _12 <- arr21 (*) -< {_6, _11}
  _13 <- arr11 (+1) -< _9
  _14 <- arr11 (+1) -< _10
  _15 <- arr21 (*) -< {_12, _13}
  _16 <- arr21 (*) -< {_11, _9}
  _17 <- pre1 0.398379359242986 -< _3
  _18 <- arr21 (+) -< {_7, _14}
  _19 <- arr21 (-) -< {_16, _4}
  _20 <- arr21 (*) -< {_19, _17}
  _21 <- arr11 (+1) -< _19
  _22 <- arr21 (-) -< {_18, _4}
  _23 <- arr11 (+1) -< _0
  _24 <- arr21 (-) -< {_21, _15}
  _25 <- arr21 (-) -< {_23, _22}
  _26 <- arr21 (+) -< {_25, _24}
  _27 <- pre1 2.3708077030536274 -< _26
  _28 <- arr21 (*) -< {_27, _20}
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 0.31772659917403234 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6