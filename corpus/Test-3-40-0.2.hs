{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _4 <- iPre 2.992844513380487 -< _1
    _5 <- FRP.Yampa.arr (+1) -< _2
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _5)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _6)
    _8 <- iPre 0.35070412296632236 -< _7
    _1 <- iPre 2.5761686946512024 -< _4
  _9 <- iPre 0.6729313608590707 -< _5
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _9)
  _11 <- FRP.Yampa.arr (+1) -< _10
  _12 <- iPre 1.241611432321775 -< _11
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _1)
  _14 <- FRP.Yampa.arr (+1) -< _2
  _15 <- FRP.Yampa.arr (+1) -< _6
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _14)
  _17 <- FRP.Yampa.arr (+1) -< _16
  _18 <- FRP.Yampa.arr (+1) -< _6
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _15)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _19)
  _21 <- iPre 0.6572749651287196 -< _19
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _21)
  _23 <- iPre 1.1072810516829976 -< _22
  _24 <- iPre 1.7661703395527755 -< _23
  _25 <- iPre 2.477976800411109 -< _20
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _10)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _20)
  _28 <- iPre 0.10681899541656671 -< _25
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _21)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _3)
  _31 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _30)
  _32 <- FRP.Yampa.arr (+1) -< _27
  _33 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _32)
  _34 <- FRP.Yampa.arr (uncurry (+)) -< (_29, _26)
  _35 <- iPre 1.2707381371268125 -< _17
  _36 <- FRP.Yampa.arr (uncurry (+)) -< (_31, _34)
  _37 <- FRP.Yampa.arr (uncurry (-)) -< (_35, _36)
  _38 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _37)
  _39 <- FRP.Yampa.arr (uncurry (-)) -< (_38, _33)
  _40 <- FRP.Yampa.arr (+1) -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_0, _0}
    _3 <- arr21 (+) -< {_0, _1}
    _4 <- pre1 2.992844513380487 -< _1
    _5 <- arr11 (+1) -< _2
    _6 <- arr21 (*) -< {_3, _5}
    _7 <- arr21 (*) -< {_0, _6}
    _8 <- pre1 0.35070412296632236 -< _7
    _1 <- pre1 2.5761686946512024 -< _4

  _9 <- pre1 0.6729313608590707 -< _5
  _10 <- arr21 (-) -< {_8, _9}
  _11 <- arr11 (+1) -< _10
  _12 <- pre1 1.241611432321775 -< _11
  _13 <- arr21 (+) -< {_9, _1}
  _14 <- arr11 (+1) -< _2
  _15 <- arr11 (+1) -< _6
  _16 <- arr21 (*) -< {_12, _14}
  _17 <- arr11 (+1) -< _16
  _18 <- arr11 (+1) -< _6
  _19 <- arr21 (*) -< {_3, _15}
  _20 <- arr21 (*) -< {_0, _19}
  _21 <- pre1 0.6572749651287196 -< _19
  _22 <- arr21 (-) -< {_8, _21}
  _23 <- pre1 1.1072810516829976 -< _22
  _24 <- pre1 1.7661703395527755 -< _23
  _25 <- pre1 2.477976800411109 -< _20
  _26 <- arr21 (+) -< {_15, _10}
  _27 <- arr21 (+) -< {_13, _20}
  _28 <- pre1 0.10681899541656671 -< _25
  _29 <- arr21 (*) -< {_18, _21}
  _30 <- arr21 (-) -< {_7, _3}
  _31 <- arr21 (+) -< {_13, _30}
  _32 <- arr11 (+1) -< _27
  _33 <- arr21 (-) -< {_24, _32}
  _34 <- arr21 (+) -< {_29, _26}
  _35 <- pre1 1.2707381371268125 -< _17
  _36 <- arr21 (+) -< {_31, _34}
  _37 <- arr21 (-) -< {_35, _36}
  _38 <- arr21 (*) -< {_28, _37}
  _39 <- arr21 (-) -< {_38, _33}
  _40 <- arr11 (+1) -< _39
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8