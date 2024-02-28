{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
  rec
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _5)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _5)
    _8 <- iPre 3.1341905590449346 -< _0
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _8)
    _4 <- iPre 3.0146116543034243 -< _9
  _10 <- FRP.Yampa.arr (+1) -< _9
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _5)
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _0)
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _10)
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _13)
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _0)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _7)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _15)
  _18 <- FRP.Yampa.arr (+1) -< _12
  _19 <- FRP.Yampa.arr (+1) -< _0
  _20 <- FRP.Yampa.arr (+1) -< _4
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _11)
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _17)
  _23 <- iPre 1.3297743610429265 -< _10
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _22)
  _25 <- iPre 2.419810776872411 -< _24
  _26 <- iPre 2.476038941799332 -< _23
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _26)
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _27)
  _29 <- FRP.Yampa.arr (+1) -< _20
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _28)
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (*) -< {_1, _0}
  _3 <- arr21 (-) -< {_1, _2}
  rec
    _5 <- arr21 (+) -< {_1, _2}
    _6 <- arr21 (-) -< {_3, _5}
    _7 <- arr21 (-) -< {_0, _5}
    _8 <- pre1 3.1341905590449346 -< _0
    _9 <- arr21 (-) -< {_4, _8}
    _4 <- pre1 3.0146116543034243 -< _9

  _10 <- arr11 (+1) -< _9
  _11 <- arr21 (*) -< {_5, _5}
  _12 <- arr21 (+) -< {_11, _0}
  _13 <- arr21 (+) -< {_6, _10}
  _14 <- arr21 (-) -< {_1, _13}
  _15 <- arr21 (*) -< {_14, _0}
  _16 <- arr21 (+) -< {_2, _7}
  _17 <- arr21 (-) -< {_2, _15}
  _18 <- arr11 (+1) -< _12
  _19 <- arr11 (+1) -< _0
  _20 <- arr11 (+1) -< _4
  _21 <- arr21 (+) -< {_16, _11}
  _22 <- arr21 (+) -< {_21, _17}
  _23 <- pre1 1.3297743610429265 -< _10
  _24 <- arr21 (-) -< {_18, _22}
  _25 <- pre1 2.419810776872411 -< _24
  _26 <- pre1 2.476038941799332 -< _23
  _27 <- arr21 (+) -< {_25, _26}
  _28 <- arr21 (-) -< {_19, _27}
  _29 <- arr11 (+1) -< _20
  _30 <- arr21 (*) -< {_29, _28}
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6