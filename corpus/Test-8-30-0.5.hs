{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
    _4 <- iPre 1.9895765598135289 -< _3
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _2)
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _5)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _4)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _0)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _7)
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _5)
    _11 <- iPre 1.097402883152416 -< _10
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _11)
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _12)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _13)
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _13)
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _4)
    _2 <- iPre 0.9649959985591868 -< _10
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _11)
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _16)
  _19 <- FRP.Yampa.arr (+1) -< _0
  _20 <- iPre 1.165269437026583 -< _9
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _20)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _11)
  _23 <- FRP.Yampa.arr (+1) -< _1
  _24 <- iPre 1.9499549347424556 -< _19
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _14)
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _24)
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _25)
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _21)
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _18)
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  rec
    _3 <- arr21 (-) -< {_1, _2}
    _4 <- pre1 1.9895765598135289 -< _3
    _5 <- arr21 (-) -< {_3, _2}
    _6 <- arr21 (+) -< {_4, _5}
    _7 <- arr21 (*) -< {_6, _4}
    _8 <- arr21 (+) -< {_7, _0}
    _9 <- arr21 (-) -< {_8, _7}
    _10 <- arr21 (-) -< {_9, _5}
    _11 <- pre1 1.097402883152416 -< _10
    _12 <- arr21 (-) -< {_9, _11}
    _13 <- arr21 (*) -< {_2, _12}
    _14 <- arr21 (+) -< {_11, _13}
    _15 <- arr21 (-) -< {_14, _13}
    _16 <- arr21 (+) -< {_15, _4}
    _2 <- pre1 0.9649959985591868 -< _10

  _17 <- arr21 (+) -< {_11, _11}
  _18 <- arr21 (-) -< {_11, _16}
  _19 <- arr11 (+1) -< _0
  _20 <- pre1 1.165269437026583 -< _9
  _21 <- arr21 (-) -< {_17, _20}
  _22 <- arr21 (-) -< {_1, _11}
  _23 <- arr11 (+1) -< _1
  _24 <- pre1 1.9499549347424556 -< _19
  _25 <- arr21 (-) -< {_11, _14}
  _26 <- arr21 (-) -< {_23, _24}
  _27 <- arr21 (*) -< {_26, _25}
  _28 <- arr21 (+) -< {_22, _21}
  _29 <- arr21 (+) -< {_27, _18}
  _30 <- arr21 (+) -< {_28, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15