{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
    _3 <- iPre 1.1727741028444125 -< _1
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
    _5 <- FRP.Yampa.arr (+1) -< _1
    _6 <- FRP.Yampa.arr (+1) -< _1
    _7 <- iPre 1.6251657619770825 -< _1
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _5)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _2)
    _10 <- FRP.Yampa.arr (+1) -< _9
    _11 <- iPre 2.000351811465281 -< _6
    _12 <- FRP.Yampa.arr (+1) -< _4
    _13 <- iPre 2.96650667837632 -< _8
    _14 <- iPre 1.0039859244981733 -< _7
    _15 <- iPre 2.9052450437594652 -< _12
    _16 <- FRP.Yampa.arr (+1) -< _11
    _17 <- FRP.Yampa.arr (+1) -< _13
    _18 <- iPre 0.9675587543840003 -< _10
    _19 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _14)
    _20 <- FRP.Yampa.arr (+1) -< _19
    _21 <- FRP.Yampa.arr (+1) -< _15
    _22 <- iPre 2.3225137234174547 -< _16
    _23 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _17)
    _24 <- FRP.Yampa.arr (+1) -< _23
    _25 <- iPre 2.290823384932473 -< _21
    _26 <- FRP.Yampa.arr (+1) -< _20
    _27 <- iPre 1.1539173810418901 -< _25
    _28 <- FRP.Yampa.arr (+1) -< _24
    _29 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _27)
    _30 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _26)
    _1 <- iPre 0.7627884682707463 -< _30
  FRP.Yampa.returnA -< _29

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_1, _1}
    _3 <- pre1 1.1727741028444125 -< _1
    _4 <- arr21 (+) -< {_1, _1}
    _5 <- arr11 (+1) -< _1
    _6 <- arr11 (+1) -< _1
    _7 <- pre1 1.6251657619770825 -< _1
    _8 <- arr21 (+) -< {_1, _5}
    _9 <- arr21 (-) -< {_3, _2}
    _10 <- arr11 (+1) -< _9
    _11 <- pre1 2.000351811465281 -< _6
    _12 <- arr11 (+1) -< _4
    _13 <- pre1 2.96650667837632 -< _8
    _14 <- pre1 1.0039859244981733 -< _7
    _15 <- pre1 2.9052450437594652 -< _12
    _16 <- arr11 (+1) -< _11
    _17 <- arr11 (+1) -< _13
    _18 <- pre1 0.9675587543840003 -< _10
    _19 <- arr21 (*) -< {_0, _14}
    _20 <- arr11 (+1) -< _19
    _21 <- arr11 (+1) -< _15
    _22 <- pre1 2.3225137234174547 -< _16
    _23 <- arr21 (-) -< {_22, _17}
    _24 <- arr11 (+1) -< _23
    _25 <- pre1 2.290823384932473 -< _21
    _26 <- arr11 (+1) -< _20
    _27 <- pre1 1.1539173810418901 -< _25
    _28 <- arr11 (+1) -< _24
    _29 <- arr21 (-) -< {_18, _27}
    _30 <- arr21 (+) -< {_28, _26}
    _1 <- pre1 0.7627884682707463 -< _30

  AFRP.returnA -< _29|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30