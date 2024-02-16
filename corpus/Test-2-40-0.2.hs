{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
  _4 <- FRP.Yampa.arr (+1) -< _3
  rec
    _6 <- FRP.Yampa.arr (+1) -< _4
    _7 <- FRP.Yampa.arr (+1) -< _5
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _4)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _8)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _7)
    _11 <- iPre 1.2781472916082803 -< _10
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _8)
    _5 <- iPre 0.2726965635632197 -< _8
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _12)
  _14 <- FRP.Yampa.arr (+1) -< _3
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _0)
  _16 <- iPre 2.9082020351497424 -< _5
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _1)
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _17)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _18)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _14)
  _21 <- FRP.Yampa.arr (+1) -< _6
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _13)
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _16)
  _24 <- iPre 1.94744954723523 -< _18
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _23)
  _26 <- FRP.Yampa.arr (+1) -< _22
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _11)
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _15)
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _21)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _26)
  _31 <- FRP.Yampa.arr (+1) -< _3
  _32 <- iPre 0.6071406965611746 -< _2
  _33 <- FRP.Yampa.arr (uncurry (-)) -< (_32, _27)
  _34 <- FRP.Yampa.arr (uncurry (-)) -< (_31, _24)
  _35 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _29)
  _36 <- FRP.Yampa.arr (+1) -< _33
  _37 <- FRP.Yampa.arr (uncurry (+)) -< (_36, _30)
  _38 <- FRP.Yampa.arr (+1) -< _34
  _39 <- FRP.Yampa.arr (uncurry (*)) -< (_38, _37)
  _40 <- FRP.Yampa.arr (uncurry (+)) -< (_35, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (-) -< {_0, _0}
  _3 <- arr21 (-) -< {_1, _2}
  _4 <- arr11 (+1) -< _3
  rec
    _6 <- arr11 (+1) -< _4
    _7 <- arr11 (+1) -< _5
    _8 <- arr21 (*) -< {_6, _4}
    _9 <- arr21 (-) -< {_0, _8}
    _10 <- arr21 (*) -< {_9, _7}
    _11 <- pre1 1.2781472916082803 -< _10
    _12 <- arr21 (*) -< {_9, _8}
    _5 <- pre1 0.2726965635632197 -< _8

  _13 <- arr21 (*) -< {_11, _12}
  _14 <- arr11 (+1) -< _3
  _15 <- arr21 (+) -< {_14, _0}
  _16 <- pre1 2.9082020351497424 -< _5
  _17 <- arr21 (+) -< {_15, _1}
  _18 <- arr21 (-) -< {_16, _17}
  _19 <- arr21 (*) -< {_6, _18}
  _20 <- arr21 (+) -< {_11, _14}
  _21 <- arr11 (+1) -< _6
  _22 <- arr21 (*) -< {_19, _13}
  _23 <- arr21 (+) -< {_0, _16}
  _24 <- pre1 1.94744954723523 -< _18
  _25 <- arr21 (+) -< {_21, _23}
  _26 <- arr11 (+1) -< _22
  _27 <- arr21 (+) -< {_20, _11}
  _28 <- arr21 (-) -< {_3, _15}
  _29 <- arr21 (-) -< {_28, _21}
  _30 <- arr21 (-) -< {_7, _26}
  _31 <- arr11 (+1) -< _3
  _32 <- pre1 0.6071406965611746 -< _2
  _33 <- arr21 (-) -< {_32, _27}
  _34 <- arr21 (-) -< {_31, _24}
  _35 <- arr21 (-) -< {_25, _29}
  _36 <- arr11 (+1) -< _33
  _37 <- arr21 (+) -< {_36, _30}
  _38 <- arr11 (+1) -< _34
  _39 <- arr21 (*) -< {_38, _37}
  _40 <- arr21 (+) -< {_35, _39}
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8