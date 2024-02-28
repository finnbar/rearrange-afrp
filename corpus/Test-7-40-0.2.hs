{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (+1) -< _1
  _4 <- iPre 0.7201591729118559 -< _3
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _2)
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _5)
  rec
    _8 <- FRP.Yampa.arr (+1) -< _2
    _9 <- iPre 1.442336113624757 -< _3
    _10 <- FRP.Yampa.arr (+1) -< _8
    _11 <- FRP.Yampa.arr (+1) -< _6
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _9)
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _7)
    _14 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _4)
    _7 <- iPre 2.917044106725332 -< _13
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _12)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _14)
  _17 <- iPre 2.5787828122972436 -< _14
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _16)
  _19 <- iPre 2.2742791123743693 -< _15
  _20 <- FRP.Yampa.arr (+1) -< _15
  _21 <- FRP.Yampa.arr (+1) -< _18
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _20)
  _23 <- iPre 0.71090177354602 -< _22
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _1)
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _6)
  _26 <- iPre 0.748738299300299 -< _19
  _27 <- FRP.Yampa.arr (+1) -< _25
  _28 <- iPre 0.9202534616086571 -< _18
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _17)
  _30 <- iPre 1.8388976871413902 -< _26
  _31 <- iPre 2.2254157777492583 -< _21
  _32 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _28)
  _33 <- iPre 2.8186644523511313 -< _30
  _34 <- FRP.Yampa.arr (+1) -< _33
  _35 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _32)
  _36 <- FRP.Yampa.arr (+1) -< _35
  _37 <- iPre 0.6276553862022995 -< _34
  _38 <- FRP.Yampa.arr (uncurry (+)) -< (_37, _36)
  _39 <- FRP.Yampa.arr (+1) -< _31
  _40 <- FRP.Yampa.arr (uncurry (-)) -< (_38, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- arr11 (+1) -< _1
  _4 <- pre1 0.7201591729118559 -< _3
  _5 <- arr21 (-) -< {_4, _2}
  _6 <- arr21 (*) -< {_3, _5}
  rec
    _8 <- arr11 (+1) -< _2
    _9 <- pre1 1.442336113624757 -< _3
    _10 <- arr11 (+1) -< _8
    _11 <- arr11 (+1) -< _6
    _12 <- arr21 (-) -< {_9, _9}
    _13 <- arr21 (+) -< {_11, _7}
    _14 <- arr21 (-) -< {_10, _4}
    _7 <- pre1 2.917044106725332 -< _13

  _15 <- arr21 (*) -< {_12, _12}
  _16 <- arr21 (*) -< {_7, _14}
  _17 <- pre1 2.5787828122972436 -< _14
  _18 <- arr21 (+) -< {_13, _16}
  _19 <- pre1 2.2742791123743693 -< _15
  _20 <- arr11 (+1) -< _15
  _21 <- arr11 (+1) -< _18
  _22 <- arr21 (-) -< {_2, _20}
  _23 <- pre1 0.71090177354602 -< _22
  _24 <- arr21 (-) -< {_23, _1}
  _25 <- arr21 (*) -< {_0, _6}
  _26 <- pre1 0.748738299300299 -< _19
  _27 <- arr11 (+1) -< _25
  _28 <- pre1 0.9202534616086571 -< _18
  _29 <- arr21 (*) -< {_27, _17}
  _30 <- pre1 1.8388976871413902 -< _26
  _31 <- pre1 2.2254157777492583 -< _21
  _32 <- arr21 (*) -< {_24, _28}
  _33 <- pre1 2.8186644523511313 -< _30
  _34 <- arr11 (+1) -< _33
  _35 <- arr21 (-) -< {_29, _32}
  _36 <- arr11 (+1) -< _35
  _37 <- pre1 0.6276553862022995 -< _34
  _38 <- arr21 (+) -< {_37, _36}
  _39 <- arr11 (+1) -< _31
  _40 <- arr21 (-) -< {_38, _39}
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8