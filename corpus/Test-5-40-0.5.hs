{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.1784507857198752 -< _0
  rec
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _4 <- iPre 0.27511327583550865 -< _1
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _5)
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
    _8 <- iPre 0.19812652550443252 -< _7
    _9 <- iPre 1.98812945298338 -< _5
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _4)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _4)
    _12 <- iPre 2.101292283895191 -< _6
    _13 <- FRP.Yampa.arr (+1) -< _5
    _14 <- FRP.Yampa.arr (+1) -< _7
    _15 <- FRP.Yampa.arr (+1) -< _3
    _16 <- FRP.Yampa.arr (+1) -< _13
    _17 <- FRP.Yampa.arr (+1) -< _2
    _18 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _12)
    _19 <- FRP.Yampa.arr (+1) -< _15
    _20 <- iPre 0.605322881201109 -< _9
    _21 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _16)
    _2 <- iPre 0.3052347641688887 -< _13
  _22 <- FRP.Yampa.arr (+1) -< _17
  _23 <- FRP.Yampa.arr (+1) -< _6
  _24 <- iPre 2.733738236731195 -< _19
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _16)
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _24)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _18)
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _14)
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _22)
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _29)
  _31 <- FRP.Yampa.arr (uncurry (*)) -< (_30, _28)
  _32 <- iPre 1.3542184070331666 -< _11
  _33 <- iPre 6.290053722305715e-2 -< _26
  _34 <- FRP.Yampa.arr (uncurry (+)) -< (_31, _8)
  _35 <- FRP.Yampa.arr (+1) -< _32
  _36 <- FRP.Yampa.arr (uncurry (-)) -< (_33, _23)
  _37 <- FRP.Yampa.arr (uncurry (*)) -< (_36, _35)
  _38 <- iPre 4.834642810808272e-2 -< _34
  _39 <- FRP.Yampa.arr (uncurry (-)) -< (_38, _37)
  _40 <- iPre 2.038408082408527 -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.1784507857198752 -< _0
  rec
    _3 <- arr21 (*) -< {_1, _1}
    _4 <- pre1 0.27511327583550865 -< _1
    _5 <- arr21 (-) -< {_0, _0}
    _6 <- arr21 (*) -< {_1, _5}
    _7 <- arr21 (+) -< {_2, _0}
    _8 <- pre1 0.19812652550443252 -< _7
    _9 <- pre1 1.98812945298338 -< _5
    _10 <- arr21 (+) -< {_0, _4}
    _11 <- arr21 (*) -< {_1, _4}
    _12 <- pre1 2.101292283895191 -< _6
    _13 <- arr11 (+1) -< _5
    _14 <- arr11 (+1) -< _7
    _15 <- arr11 (+1) -< _3
    _16 <- arr11 (+1) -< _13
    _17 <- arr11 (+1) -< _2
    _18 <- arr21 (-) -< {_12, _12}
    _19 <- arr11 (+1) -< _15
    _20 <- pre1 0.605322881201109 -< _9
    _21 <- arr21 (+) -< {_12, _16}
    _2 <- pre1 0.3052347641688887 -< _13

  _22 <- arr11 (+1) -< _17
  _23 <- arr11 (+1) -< _6
  _24 <- pre1 2.733738236731195 -< _19
  _25 <- arr21 (*) -< {_10, _16}
  _26 <- arr21 (+) -< {_25, _24}
  _27 <- arr21 (+) -< {_1, _18}
  _28 <- arr21 (+) -< {_20, _14}
  _29 <- arr21 (+) -< {_21, _22}
  _30 <- arr21 (+) -< {_27, _29}
  _31 <- arr21 (*) -< {_30, _28}
  _32 <- pre1 1.3542184070331666 -< _11
  _33 <- pre1 6.290053722305715e-2 -< _26
  _34 <- arr21 (+) -< {_31, _8}
  _35 <- arr11 (+1) -< _32
  _36 <- arr21 (-) -< {_33, _23}
  _37 <- arr21 (*) -< {_36, _35}
  _38 <- pre1 4.834642810808272e-2 -< _34
  _39 <- arr21 (-) -< {_38, _37}
  _40 <- pre1 2.038408082408527 -< _39
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20