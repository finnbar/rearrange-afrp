{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- iPre 0.5254197486190265 -< _1
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _4 <- FRP.Yampa.arr (+1) -< _3
  _5 <- FRP.Yampa.arr (+1) -< _1
  _6 <- FRP.Yampa.arr (+1) -< _5
  _7 <- iPre 1.3075265415037807 -< _2
  _8 <- iPre 0.402549318155218 -< _5
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _4)
  _11 <- FRP.Yampa.arr (+1) -< _7
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _2)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _12)
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _3)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _12)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _5)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _4)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _11)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _18)
  _20 <- iPre 0.5256426398432467 -< _14
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _5)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _21)
  _23 <- FRP.Yampa.arr (+1) -< _14
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _17)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _12)
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _7)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _19)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _22)
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _28)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _27)
  _31 <- iPre 2.2677683517680056 -< _23
  _32 <- FRP.Yampa.arr (+1) -< _26
  _33 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _32)
  _34 <- FRP.Yampa.arr (+1) -< _25
  _35 <- FRP.Yampa.arr (uncurry (-)) -< (_34, _31)
  _36 <- FRP.Yampa.arr (+1) -< _16
  _37 <- FRP.Yampa.arr (uncurry (*)) -< (_35, _36)
  _38 <- iPre 1.825238560442411 -< _33
  _39 <- FRP.Yampa.arr (uncurry (-)) -< (_38, _37)
  _40 <- FRP.Yampa.arr (+1) -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- pre1 0.5254197486190265 -< _1
  _3 <- arr21 (+) -< {_1, _0}
  _4 <- arr11 (+1) -< _3
  _5 <- arr11 (+1) -< _1
  _6 <- arr11 (+1) -< _5
  _7 <- pre1 1.3075265415037807 -< _2
  _8 <- pre1 0.402549318155218 -< _5
  _9 <- arr21 (+) -< {_2, _0}
  _10 <- arr21 (+) -< {_9, _4}
  _11 <- arr11 (+1) -< _7
  _12 <- arr21 (-) -< {_5, _2}
  _13 <- arr21 (*) -< {_9, _12}
  _14 <- arr21 (-) -< {_13, _3}
  _15 <- arr21 (+) -< {_8, _12}
  _16 <- arr21 (+) -< {_15, _5}
  _17 <- arr21 (+) -< {_9, _4}
  _18 <- arr21 (*) -< {_6, _11}
  _19 <- arr21 (*) -< {_2, _18}
  _20 <- pre1 0.5256426398432467 -< _14
  _21 <- arr21 (-) -< {_20, _5}
  _22 <- arr21 (-) -< {_8, _21}
  _23 <- arr11 (+1) -< _14
  _24 <- arr21 (*) -< {_12, _17}
  _25 <- arr21 (-) -< {_10, _12}
  _26 <- arr21 (-) -< {_10, _7}
  _27 <- arr21 (+) -< {_20, _19}
  _28 <- arr21 (*) -< {_24, _22}
  _29 <- arr21 (+) -< {_0, _28}
  _30 <- arr21 (-) -< {_29, _27}
  _31 <- pre1 2.2677683517680056 -< _23
  _32 <- arr11 (+1) -< _26
  _33 <- arr21 (+) -< {_30, _32}
  _34 <- arr11 (+1) -< _25
  _35 <- arr21 (-) -< {_34, _31}
  _36 <- arr11 (+1) -< _16
  _37 <- arr21 (*) -< {_35, _36}
  _38 <- pre1 1.825238560442411 -< _33
  _39 <- arr21 (-) -< {_38, _37}
  _40 <- arr11 (+1) -< _39
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0