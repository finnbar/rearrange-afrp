{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- iPre 2.9112088309924373 -< _0
  rec
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _0)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _5)
    _7 <- FRP.Yampa.arr (+1) -< _2
    _8 <- iPre 1.5847835311592808 -< _3
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _7)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _6)
    _3 <- iPre 2.2817405210217783 -< _6
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _9)
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _11)
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _5)
  _14 <- iPre 3.3972946121777947 -< _3
  _15 <- FRP.Yampa.arr (+1) -< _12
  _16 <- iPre 1.5519662299782098 -< _8
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _16)
  _18 <- iPre 2.9370970902832076 -< _13
  _19 <- iPre 0.7097566225131455 -< _15
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _11)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _4)
  _22 <- iPre 0.705725697002409 -< _2
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _2)
  _24 <- iPre 0.38265797843881955 -< _4
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _20)
  _26 <- FRP.Yampa.arr (+1) -< _17
  _27 <- iPre 0.4122762148936647 -< _22
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _18)
  _29 <- iPre 0.8732272541566607 -< _27
  _30 <- FRP.Yampa.arr (+1) -< _25
  _31 <- FRP.Yampa.arr (+1) -< _23
  _32 <- FRP.Yampa.arr (uncurry (-)) -< (_31, _29)
  _33 <- FRP.Yampa.arr (uncurry (+)) -< (_32, _24)
  _34 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _28)
  _35 <- FRP.Yampa.arr (uncurry (-)) -< (_33, _30)
  _36 <- FRP.Yampa.arr (uncurry (-)) -< (_34, _14)
  _37 <- FRP.Yampa.arr (+1) -< _36
  _38 <- FRP.Yampa.arr (uncurry (*)) -< (_37, _35)
  _39 <- FRP.Yampa.arr (+1) -< _38
  _40 <- FRP.Yampa.arr (+1) -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- pre1 2.9112088309924373 -< _0
  rec
    _4 <- arr21 (+) -< {_3, _0}
    _5 <- arr21 (-) -< {_0, _2}
    _6 <- arr21 (+) -< {_1, _5}
    _7 <- arr11 (+1) -< _2
    _8 <- pre1 1.5847835311592808 -< _3
    _9 <- arr21 (-) -< {_6, _7}
    _10 <- arr21 (+) -< {_3, _6}
    _3 <- pre1 2.2817405210217783 -< _6

  _11 <- arr21 (+) -< {_3, _9}
  _12 <- arr21 (*) -< {_8, _11}
  _13 <- arr21 (+) -< {_6, _5}
  _14 <- pre1 3.3972946121777947 -< _3
  _15 <- arr11 (+1) -< _12
  _16 <- pre1 1.5519662299782098 -< _8
  _17 <- arr21 (*) -< {_13, _16}
  _18 <- pre1 2.9370970902832076 -< _13
  _19 <- pre1 0.7097566225131455 -< _15
  _20 <- arr21 (+) -< {_7, _11}
  _21 <- arr21 (+) -< {_1, _4}
  _22 <- pre1 0.705725697002409 -< _2
  _23 <- arr21 (-) -< {_10, _2}
  _24 <- pre1 0.38265797843881955 -< _4
  _25 <- arr21 (+) -< {_21, _20}
  _26 <- arr11 (+1) -< _17
  _27 <- pre1 0.4122762148936647 -< _22
  _28 <- arr21 (-) -< {_26, _18}
  _29 <- pre1 0.8732272541566607 -< _27
  _30 <- arr11 (+1) -< _25
  _31 <- arr11 (+1) -< _23
  _32 <- arr21 (-) -< {_31, _29}
  _33 <- arr21 (+) -< {_32, _24}
  _34 <- arr21 (+) -< {_19, _28}
  _35 <- arr21 (-) -< {_33, _30}
  _36 <- arr21 (-) -< {_34, _14}
  _37 <- arr11 (+1) -< _36
  _38 <- arr21 (*) -< {_37, _35}
  _39 <- arr11 (+1) -< _38
  _40 <- arr11 (+1) -< _39
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8