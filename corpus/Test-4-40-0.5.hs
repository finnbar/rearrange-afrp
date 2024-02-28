{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (+1) -< _0
  _4 <- iPre 1.6599624680465295 -< _2
  _5 <- iPre 0.3712816219751785 -< _3
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _5)
  rec
    _8 <- iPre 1.0786722470732573 -< _6
    _9 <- FRP.Yampa.arr (+1) -< _7
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _7)
    _11 <- iPre 2.173553157145741 -< _4
    _12 <- iPre 2.697299653618098 -< _3
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _4)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _5)
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _9)
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _14)
    _17 <- iPre 2.280259146658983 -< _4
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _10)
    _19 <- FRP.Yampa.arr (+1) -< _0
    _20 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _1)
    _21 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _11)
    _22 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _8)
    _23 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _10)
    _24 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _20)
    _25 <- iPre 0.9518984778027341 -< _2
    _26 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _5)
    _7 <- iPre 1.0950304560886466 -< _23
  _27 <- iPre 1.7583946566599638 -< _19
  _28 <- iPre 1.3930280023755814 -< _21
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _18)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _13)
  _31 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _25)
  _32 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _30)
  _33 <- FRP.Yampa.arr (+1) -< _31
  _34 <- iPre 3.0329237335386106 -< _29
  _35 <- FRP.Yampa.arr (+1) -< _34
  _36 <- iPre 0.11975353182066986 -< _35
  _37 <- FRP.Yampa.arr (uncurry (+)) -< (_36, _32)
  _38 <- FRP.Yampa.arr (uncurry (-)) -< (_33, _37)
  _39 <- FRP.Yampa.arr (+1) -< _38
  _40 <- iPre 0.8846249861561482 -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr21 (*) -< {_0, _1}
  _3 <- arr11 (+1) -< _0
  _4 <- pre1 1.6599624680465295 -< _2
  _5 <- pre1 0.3712816219751785 -< _3
  _6 <- arr21 (+) -< {_0, _5}
  rec
    _8 <- pre1 1.0786722470732573 -< _6
    _9 <- arr11 (+1) -< _7
    _10 <- arr21 (-) -< {_7, _7}
    _11 <- pre1 2.173553157145741 -< _4
    _12 <- pre1 2.697299653618098 -< _3
    _13 <- arr21 (-) -< {_5, _4}
    _14 <- arr21 (*) -< {_12, _5}
    _15 <- arr21 (+) -< {_4, _9}
    _16 <- arr21 (+) -< {_4, _14}
    _17 <- pre1 2.280259146658983 -< _4
    _18 <- arr21 (*) -< {_10, _10}
    _19 <- arr11 (+1) -< _0
    _20 <- arr21 (*) -< {_4, _1}
    _21 <- arr21 (*) -< {_16, _11}
    _22 <- arr21 (-) -< {_17, _8}
    _23 <- arr21 (*) -< {_22, _10}
    _24 <- arr21 (+) -< {_8, _20}
    _25 <- pre1 0.9518984778027341 -< _2
    _26 <- arr21 (+) -< {_15, _5}
    _7 <- pre1 1.0950304560886466 -< _23

  _27 <- pre1 1.7583946566599638 -< _19
  _28 <- pre1 1.3930280023755814 -< _21
  _29 <- arr21 (*) -< {_27, _18}
  _30 <- arr21 (*) -< {_24, _13}
  _31 <- arr21 (*) -< {_26, _25}
  _32 <- arr21 (-) -< {_28, _30}
  _33 <- arr11 (+1) -< _31
  _34 <- pre1 3.0329237335386106 -< _29
  _35 <- arr11 (+1) -< _34
  _36 <- pre1 0.11975353182066986 -< _35
  _37 <- arr21 (+) -< {_36, _32}
  _38 <- arr21 (-) -< {_33, _37}
  _39 <- arr11 (+1) -< _38
  _40 <- pre1 0.8846249861561482 -< _39
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20