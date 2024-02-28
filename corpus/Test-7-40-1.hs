{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _3 <- FRP.Yampa.arr (+1) -< _2
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _5)
    _7 <- iPre 1.9078007316548329 -< _4
    _8 <- FRP.Yampa.arr (+1) -< _3
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _1)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _9)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _8)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _5)
    _13 <- iPre 0.16148066092166366 -< _5
    _14 <- FRP.Yampa.arr (+1) -< _6
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _1)
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _15)
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _9)
    _18 <- FRP.Yampa.arr (+1) -< _15
    _19 <- iPre 0.1806058053410013 -< _10
    _20 <- FRP.Yampa.arr (+1) -< _12
    _21 <- iPre 2.7188605026256196 -< _14
    _22 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _18)
    _23 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _22)
    _24 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _19)
    _25 <- iPre 1.3069943192805262 -< _6
    _26 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _23)
    _27 <- FRP.Yampa.arr (+1) -< _24
    _28 <- FRP.Yampa.arr (+1) -< _19
    _29 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _21)
    _30 <- iPre 2.318687076053871 -< _26
    _31 <- iPre 1.3953258895821248 -< _28
    _32 <- FRP.Yampa.arr (+1) -< _25
    _33 <- FRP.Yampa.arr (+1) -< _30
    _34 <- FRP.Yampa.arr (uncurry (*)) -< (_32, _16)
    _35 <- FRP.Yampa.arr (uncurry (*)) -< (_31, _29)
    _36 <- iPre 2.1094630243648447 -< _33
    _37 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _35)
    _38 <- FRP.Yampa.arr (+1) -< _37
    _39 <- FRP.Yampa.arr (uncurry (*)) -< (_34, _38)
    _40 <- iPre 2.539609576582914 -< _39
    _1 <- iPre 2.3638103801271466 -< _40
  FRP.Yampa.returnA -< _36

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_1, _1}
    _3 <- arr11 (+1) -< _2
    _4 <- arr21 (-) -< {_1, _0}
    _5 <- arr21 (*) -< {_0, _0}
    _6 <- arr21 (*) -< {_2, _5}
    _7 <- pre1 1.9078007316548329 -< _4
    _8 <- arr11 (+1) -< _3
    _9 <- arr21 (-) -< {_8, _1}
    _10 <- arr21 (*) -< {_4, _9}
    _11 <- arr21 (*) -< {_7, _8}
    _12 <- arr21 (*) -< {_11, _5}
    _13 <- pre1 0.16148066092166366 -< _5
    _14 <- arr11 (+1) -< _6
    _15 <- arr21 (+) -< {_2, _1}
    _16 <- arr21 (+) -< {_13, _15}
    _17 <- arr21 (*) -< {_14, _9}
    _18 <- arr11 (+1) -< _15
    _19 <- pre1 0.1806058053410013 -< _10
    _20 <- arr11 (+1) -< _12
    _21 <- pre1 2.7188605026256196 -< _14
    _22 <- arr21 (*) -< {_18, _18}
    _23 <- arr21 (+) -< {_20, _22}
    _24 <- arr21 (+) -< {_19, _19}
    _25 <- pre1 1.3069943192805262 -< _6
    _26 <- arr21 (*) -< {_9, _23}
    _27 <- arr11 (+1) -< _24
    _28 <- arr11 (+1) -< _19
    _29 <- arr21 (-) -< {_17, _21}
    _30 <- pre1 2.318687076053871 -< _26
    _31 <- pre1 1.3953258895821248 -< _28
    _32 <- arr11 (+1) -< _25
    _33 <- arr11 (+1) -< _30
    _34 <- arr21 (*) -< {_32, _16}
    _35 <- arr21 (*) -< {_31, _29}
    _36 <- pre1 2.1094630243648447 -< _33
    _37 <- arr21 (+) -< {_27, _35}
    _38 <- arr11 (+1) -< _37
    _39 <- arr21 (*) -< {_34, _38}
    _40 <- pre1 2.539609576582914 -< _39
    _1 <- pre1 2.3638103801271466 -< _40

  AFRP.returnA -< _36|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40