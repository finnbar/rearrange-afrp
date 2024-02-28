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
    _3 <- FRP.Yampa.arr (+1) -< _2
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _2)
    _5 <- FRP.Yampa.arr (+1) -< _0
    _6 <- FRP.Yampa.arr (+1) -< _4
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _5)
    _8 <- FRP.Yampa.arr (+1) -< _4
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _4)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _2)
    _11 <- iPre 2.170772749988114 -< _7
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _3)
    _13 <- FRP.Yampa.arr (+1) -< _12
    _14 <- iPre 0.36187357262503816 -< _9
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _1)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _5)
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _0)
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _13)
    _19 <- iPre 2.442427181334901 -< _11
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _19)
    _21 <- iPre 2.626002951279859 -< _17
    _22 <- FRP.Yampa.arr (+1) -< _20
    _23 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _22)
    _24 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _23)
    _25 <- iPre 0.5710128138544343 -< _21
    _26 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _6)
    _27 <- FRP.Yampa.arr (+1) -< _24
    _28 <- iPre 1.2351565917111473 -< _27
    _29 <- FRP.Yampa.arr (+1) -< _28
    _30 <- iPre 1.1988245331339347 -< _29
    _1 <- iPre 0.2749213711200922 -< _26
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_0, _0}
    _3 <- arr11 (+1) -< _2
    _4 <- arr21 (+) -< {_3, _2}
    _5 <- arr11 (+1) -< _0
    _6 <- arr11 (+1) -< _4
    _7 <- arr21 (+) -< {_3, _5}
    _8 <- arr11 (+1) -< _4
    _9 <- arr21 (-) -< {_8, _4}
    _10 <- arr21 (*) -< {_7, _2}
    _11 <- pre1 2.170772749988114 -< _7
    _12 <- arr21 (-) -< {_10, _3}
    _13 <- arr11 (+1) -< _12
    _14 <- pre1 0.36187357262503816 -< _9
    _15 <- arr21 (-) -< {_5, _1}
    _16 <- arr21 (*) -< {_9, _5}
    _17 <- arr21 (*) -< {_4, _0}
    _18 <- arr21 (*) -< {_14, _13}
    _19 <- pre1 2.442427181334901 -< _11
    _20 <- arr21 (+) -< {_16, _19}
    _21 <- pre1 2.626002951279859 -< _17
    _22 <- arr11 (+1) -< _20
    _23 <- arr21 (*) -< {_15, _22}
    _24 <- arr21 (-) -< {_18, _23}
    _25 <- pre1 0.5710128138544343 -< _21
    _26 <- arr21 (-) -< {_25, _6}
    _27 <- arr11 (+1) -< _24
    _28 <- pre1 1.2351565917111473 -< _27
    _29 <- arr11 (+1) -< _28
    _30 <- pre1 1.1988245331339347 -< _29
    _1 <- pre1 0.2749213711200922 -< _26

  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30