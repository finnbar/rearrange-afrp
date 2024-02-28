{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- iPre 1.1969236418889857 -< _0
  _3 <- FRP.Yampa.arr (+1) -< _0
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  rec
    _6 <- FRP.Yampa.arr (+1) -< _0
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _4)
    _8 <- iPre 2.2263978448734005 -< _5
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _4)
    _10 <- iPre 0.2364029289235711 -< _4
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _0)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _0)
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _12)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _12)
    _15 <- iPre 1.6435953520698785 -< _1
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _5)
    _17 <- iPre 0.8900841005422708 -< _0
    _18 <- iPre 2.109902356022022 -< _6
    _19 <- FRP.Yampa.arr (+1) -< _1
    _20 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _16)
    _21 <- iPre 2.8851001253251396 -< _9
    _22 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _6)
    _23 <- iPre 2.715617112615526 -< _21
    _24 <- iPre 2.3052856207879766 -< _3
    _5 <- iPre 0.474171499002411 -< _15
  _25 <- iPre 0.8388789879244288 -< _17
  _26 <- FRP.Yampa.arr (+1) -< _20
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _7)
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _19)
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _23)
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _8)
  _31 <- FRP.Yampa.arr (+1) -< _27
  _32 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _10)
  _33 <- FRP.Yampa.arr (uncurry (*)) -< (_30, _28)
  _34 <- FRP.Yampa.arr (uncurry (+)) -< (_32, _31)
  _35 <- FRP.Yampa.arr (+1) -< _33
  _36 <- FRP.Yampa.arr (uncurry (-)) -< (_34, _18)
  _37 <- iPre 0.9722488855376985 -< _35
  _38 <- iPre 1.4264810536873258 -< _37
  _39 <- FRP.Yampa.arr (uncurry (+)) -< (_38, _36)
  _40 <- iPre 1.54929723333244 -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- pre1 1.1969236418889857 -< _0
  _3 <- arr11 (+1) -< _0
  _4 <- arr21 (*) -< {_0, _0}
  rec
    _6 <- arr11 (+1) -< _0
    _7 <- arr21 (+) -< {_4, _4}
    _8 <- pre1 2.2263978448734005 -< _5
    _9 <- arr21 (*) -< {_3, _4}
    _10 <- pre1 0.2364029289235711 -< _4
    _11 <- arr21 (*) -< {_4, _0}
    _12 <- arr21 (-) -< {_11, _0}
    _13 <- arr21 (*) -< {_4, _12}
    _14 <- arr21 (+) -< {_2, _12}
    _15 <- pre1 1.6435953520698785 -< _1
    _16 <- arr21 (-) -< {_4, _5}
    _17 <- pre1 0.8900841005422708 -< _0
    _18 <- pre1 2.109902356022022 -< _6
    _19 <- arr11 (+1) -< _1
    _20 <- arr21 (*) -< {_14, _16}
    _21 <- pre1 2.8851001253251396 -< _9
    _22 <- arr21 (*) -< {_13, _6}
    _23 <- pre1 2.715617112615526 -< _21
    _24 <- pre1 2.3052856207879766 -< _3
    _5 <- pre1 0.474171499002411 -< _15

  _25 <- pre1 0.8388789879244288 -< _17
  _26 <- arr11 (+1) -< _20
  _27 <- arr21 (*) -< {_25, _7}
  _28 <- arr21 (-) -< {_22, _19}
  _29 <- arr21 (+) -< {_26, _23}
  _30 <- arr21 (+) -< {_24, _8}
  _31 <- arr11 (+1) -< _27
  _32 <- arr21 (*) -< {_29, _10}
  _33 <- arr21 (*) -< {_30, _28}
  _34 <- arr21 (+) -< {_32, _31}
  _35 <- arr11 (+1) -< _33
  _36 <- arr21 (-) -< {_34, _18}
  _37 <- pre1 0.9722488855376985 -< _35
  _38 <- pre1 1.4264810536873258 -< _37
  _39 <- arr21 (+) -< {_38, _36}
  _40 <- pre1 1.54929723333244 -< _39
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20