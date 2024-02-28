{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- iPre 0.8367110945684325 -< _2
  _4 <- FRP.Yampa.arr (+1) -< _0
  _5 <- FRP.Yampa.arr (+1) -< _0
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _5)
  _7 <- FRP.Yampa.arr (+1) -< _1
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _7)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _4)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _8)
  _11 <- iPre 2.91121305640955 -< _8
  _12 <- iPre 0.9151548367163573 -< _0
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _9)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _13)
  _15 <- FRP.Yampa.arr (+1) -< _6
  _16 <- FRP.Yampa.arr (+1) -< _10
  _17 <- iPre 1.380463837840816 -< _15
  _18 <- iPre 2.978337209308067 -< _6
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _3)
  _20 <- iPre 2.2396040755920916 -< _14
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _14)
  _22 <- FRP.Yampa.arr (+1) -< _19
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _22)
  _24 <- FRP.Yampa.arr (+1) -< _12
  _25 <- FRP.Yampa.arr (+1) -< _1
  _26 <- iPre 2.6776485260935425 -< _8
  _27 <- FRP.Yampa.arr (+1) -< _25
  _28 <- FRP.Yampa.arr (+1) -< _16
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _26)
  _30 <- FRP.Yampa.arr (+1) -< _11
  _31 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _29)
  _32 <- iPre 1.334319465372436 -< _21
  _33 <- FRP.Yampa.arr (uncurry (-)) -< (_27, _24)
  _34 <- FRP.Yampa.arr (+1) -< _32
  _35 <- iPre 2.9505944512487097 -< _30
  _36 <- FRP.Yampa.arr (uncurry (+)) -< (_35, _34)
  _37 <- FRP.Yampa.arr (uncurry (-)) -< (_36, _33)
  _38 <- iPre 0.9262981961105364 -< _31
  _39 <- FRP.Yampa.arr (uncurry (*)) -< (_38, _37)
  _40 <- FRP.Yampa.arr (+1) -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  _3 <- pre1 0.8367110945684325 -< _2
  _4 <- arr11 (+1) -< _0
  _5 <- arr11 (+1) -< _0
  _6 <- arr21 (-) -< {_4, _5}
  _7 <- arr11 (+1) -< _1
  _8 <- arr21 (*) -< {_6, _7}
  _9 <- arr21 (+) -< {_1, _4}
  _10 <- arr21 (*) -< {_3, _8}
  _11 <- pre1 2.91121305640955 -< _8
  _12 <- pre1 0.9151548367163573 -< _0
  _13 <- arr21 (-) -< {_4, _9}
  _14 <- arr21 (+) -< {_12, _13}
  _15 <- arr11 (+1) -< _6
  _16 <- arr11 (+1) -< _10
  _17 <- pre1 1.380463837840816 -< _15
  _18 <- pre1 2.978337209308067 -< _6
  _19 <- arr21 (+) -< {_18, _3}
  _20 <- pre1 2.2396040755920916 -< _14
  _21 <- arr21 (*) -< {_17, _14}
  _22 <- arr11 (+1) -< _19
  _23 <- arr21 (-) -< {_20, _22}
  _24 <- arr11 (+1) -< _12
  _25 <- arr11 (+1) -< _1
  _26 <- pre1 2.6776485260935425 -< _8
  _27 <- arr11 (+1) -< _25
  _28 <- arr11 (+1) -< _16
  _29 <- arr21 (+) -< {_23, _26}
  _30 <- arr11 (+1) -< _11
  _31 <- arr21 (-) -< {_28, _29}
  _32 <- pre1 1.334319465372436 -< _21
  _33 <- arr21 (-) -< {_27, _24}
  _34 <- arr11 (+1) -< _32
  _35 <- pre1 2.9505944512487097 -< _30
  _36 <- arr21 (+) -< {_35, _34}
  _37 <- arr21 (-) -< {_36, _33}
  _38 <- pre1 0.9262981961105364 -< _31
  _39 <- arr21 (*) -< {_38, _37}
  _40 <- arr11 (+1) -< _39
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0