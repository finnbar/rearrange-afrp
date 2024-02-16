{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- iPre 2.3844353889058785 -< _0
  _4 <- iPre 1.8281824891111291 -< _0
  rec
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _3)
    _7 <- iPre 0.42152818971376493 -< _1
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _6)
    _9 <- FRP.Yampa.arr (+1) -< _7
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _8)
    _5 <- iPre 4.875375370943679e-2 -< _5
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _10)
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _11)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _3)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _5)
  _15 <- iPre 1.1124041572100194 -< _2
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _14)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _16)
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _17)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _8)
  _20 <- FRP.Yampa.arr (+1) -< _3
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _20)
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _20)
  _23 <- iPre 1.5096677978778525e-2 -< _22
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _23)
  _25 <- FRP.Yampa.arr (+1) -< _24
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _1)
  _27 <- iPre 0.3102167191493062 -< _12
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _27)
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _21)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _5)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr11 (+1) -< _0
  _3 <- pre1 2.3844353889058785 -< _0
  _4 <- pre1 1.8281824891111291 -< _0
  rec
    _6 <- arr21 (*) -< {_0, _3}
    _7 <- pre1 0.42152818971376493 -< _1
    _8 <- arr21 (-) -< {_4, _6}
    _9 <- arr11 (+1) -< _7
    _10 <- arr21 (+) -< {_2, _8}
    _5 <- pre1 4.875375370943679e-2 -< _5

  _11 <- arr21 (*) -< {_3, _10}
  _12 <- arr21 (+) -< {_9, _11}
  _13 <- arr21 (*) -< {_12, _3}
  _14 <- arr21 (+) -< {_13, _5}
  _15 <- pre1 1.1124041572100194 -< _2
  _16 <- arr21 (-) -< {_8, _14}
  _17 <- arr21 (+) -< {_8, _16}
  _18 <- arr21 (-) -< {_15, _17}
  _19 <- arr21 (*) -< {_18, _8}
  _20 <- arr11 (+1) -< _3
  _21 <- arr21 (-) -< {_19, _20}
  _22 <- arr21 (*) -< {_21, _20}
  _23 <- pre1 1.5096677978778525e-2 -< _22
  _24 <- arr21 (+) -< {_0, _23}
  _25 <- arr11 (+1) -< _24
  _26 <- arr21 (-) -< {_25, _1}
  _27 <- pre1 0.3102167191493062 -< _12
  _28 <- arr21 (*) -< {_26, _27}
  _29 <- arr21 (+) -< {_28, _21}
  _30 <- arr21 (*) -< {_29, _5}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6