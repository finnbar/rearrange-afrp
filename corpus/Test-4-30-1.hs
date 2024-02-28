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
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _0)
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _0)
    _5 <- iPre 1.738737088964429 -< _0
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _5)
    _7 <- iPre 0.983983028152095 -< _6
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _6)
    _9 <- FRP.Yampa.arr (+1) -< _5
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _6)
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _1)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _5)
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _12)
    _14 <- iPre 2.9471221180873163e-2 -< _13
    _15 <- iPre 0.12136631817180049 -< _5
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _8)
    _17 <- FRP.Yampa.arr (+1) -< _16
    _18 <- iPre 2.92019000437663 -< _15
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _17)
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _19)
    _21 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _7)
    _22 <- FRP.Yampa.arr (+1) -< _15
    _23 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _22)
    _24 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _20)
    _25 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _18)
    _26 <- FRP.Yampa.arr (+1) -< _25
    _27 <- FRP.Yampa.arr (+1) -< _24
    _28 <- iPre 0.8875286752433662 -< _26
    _29 <- FRP.Yampa.arr (+1) -< _27
    _30 <- FRP.Yampa.arr (+1) -< _29
    _1 <- iPre 1.5064739713632522 -< _30
  FRP.Yampa.returnA -< _28

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_1, _0}
    _3 <- arr21 (*) -< {_2, _0}
    _4 <- arr21 (+) -< {_3, _0}
    _5 <- pre1 1.738737088964429 -< _0
    _6 <- arr21 (+) -< {_4, _5}
    _7 <- pre1 0.983983028152095 -< _6
    _8 <- arr21 (*) -< {_5, _6}
    _9 <- arr11 (+1) -< _5
    _10 <- arr21 (*) -< {_5, _6}
    _11 <- arr21 (-) -< {_9, _1}
    _12 <- arr21 (-) -< {_7, _5}
    _13 <- arr21 (*) -< {_10, _12}
    _14 <- pre1 2.9471221180873163e-2 -< _13
    _15 <- pre1 0.12136631817180049 -< _5
    _16 <- arr21 (*) -< {_11, _8}
    _17 <- arr11 (+1) -< _16
    _18 <- pre1 2.92019000437663 -< _15
    _19 <- arr21 (+) -< {_14, _17}
    _20 <- arr21 (+) -< {_3, _19}
    _21 <- arr21 (+) -< {_1, _7}
    _22 <- arr11 (+1) -< _15
    _23 <- arr21 (-) -< {_12, _22}
    _24 <- arr21 (*) -< {_23, _20}
    _25 <- arr21 (-) -< {_21, _18}
    _26 <- arr11 (+1) -< _25
    _27 <- arr11 (+1) -< _24
    _28 <- pre1 0.8875286752433662 -< _26
    _29 <- arr11 (+1) -< _27
    _30 <- arr11 (+1) -< _29
    _1 <- pre1 1.5064739713632522 -< _30

  AFRP.returnA -< _28|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30