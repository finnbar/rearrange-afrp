{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _1)
    _4 <- iPre 1.1959582552213374e-2 -< _3
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _0)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _4)
    _9 <- FRP.Yampa.arr (+1) -< _1
    _10 <- iPre 0.23185023714485017 -< _8
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _6)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _7)
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _9)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _13)
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _2)
    _16 <- iPre 2.7275535990162445 -< _4
    _17 <- FRP.Yampa.arr (+1) -< _1
    _18 <- iPre 2.3525636684903124 -< _17
    _19 <- FRP.Yampa.arr (+1) -< _14
    _20 <- iPre 2.8253038429296233 -< _12
    _21 <- iPre 1.9851964079623958 -< _13
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _15)
    _23 <- FRP.Yampa.arr (+1) -< _16
    _24 <- FRP.Yampa.arr (+1) -< _20
    _25 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _24)
    _26 <- FRP.Yampa.arr (+1) -< _25
    _27 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _22)
    _28 <- FRP.Yampa.arr (+1) -< _19
    _29 <- FRP.Yampa.arr (+1) -< _28
    _30 <- FRP.Yampa.arr (uncurry (-)) -< (_27, _29)
    _1 <- iPre 1.6448072983374895 -< _26
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_0, _0}
    _3 <- arr21 (+) -< {_2, _1}
    _4 <- pre1 1.1959582552213374e-2 -< _3
    _5 <- arr21 (+) -< {_2, _2}
    _6 <- arr21 (*) -< {_5, _0}
    _7 <- arr21 (*) -< {_0, _1}
    _8 <- arr21 (*) -< {_3, _4}
    _9 <- arr11 (+1) -< _1
    _10 <- pre1 0.23185023714485017 -< _8
    _11 <- arr21 (*) -< {_10, _6}
    _12 <- arr21 (-) -< {_11, _7}
    _13 <- arr21 (-) -< {_1, _9}
    _14 <- arr21 (+) -< {_12, _13}
    _15 <- arr21 (*) -< {_4, _2}
    _16 <- pre1 2.7275535990162445 -< _4
    _17 <- arr11 (+1) -< _1
    _18 <- pre1 2.3525636684903124 -< _17
    _19 <- arr11 (+1) -< _14
    _20 <- pre1 2.8253038429296233 -< _12
    _21 <- pre1 1.9851964079623958 -< _13
    _22 <- arr21 (+) -< {_21, _15}
    _23 <- arr11 (+1) -< _16
    _24 <- arr11 (+1) -< _20
    _25 <- arr21 (+) -< {_18, _24}
    _26 <- arr11 (+1) -< _25
    _27 <- arr21 (+) -< {_23, _22}
    _28 <- arr11 (+1) -< _19
    _29 <- arr11 (+1) -< _28
    _30 <- arr21 (-) -< {_27, _29}
    _1 <- pre1 1.6448072983374895 -< _26

  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30