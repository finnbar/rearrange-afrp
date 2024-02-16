{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _0)
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _5)
  _7 <- FRP.Yampa.arr (+1) -< _4
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _4)
  _9 <- iPre 2.661918922156457 -< _4
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _3)
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _10)
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _11)
  _13 <- FRP.Yampa.arr (+1) -< _12
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _13)
  _15 <- FRP.Yampa.arr (+1) -< _14
  _16 <- FRP.Yampa.arr (+1) -< _15
  _17 <- FRP.Yampa.arr (+1) -< _16
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _17)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _4)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _7)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _10)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _2)
  _23 <- FRP.Yampa.arr (+1) -< _22
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _23)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _7)
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _17)
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _25)
  _28 <- iPre 0.6814855198863705 -< _6
  _29 <- FRP.Yampa.arr (+1) -< _2
  _30 <- FRP.Yampa.arr (+1) -< _24
  _31 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _12)
  _32 <- iPre 0.5076694795547356 -< _5
  _33 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _21)
  _34 <- FRP.Yampa.arr (+1) -< _31
  _35 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _33)
  _36 <- FRP.Yampa.arr (uncurry (*)) -< (_30, _32)
  _37 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _36)
  _38 <- FRP.Yampa.arr (uncurry (-)) -< (_35, _37)
  _39 <- iPre 0.2459658880181843 -< _34
  _40 <- FRP.Yampa.arr (uncurry (+)) -< (_38, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- arr11 (+1) -< _2
  _4 <- arr21 (+) -< {_1, _0}
  _5 <- arr21 (-) -< {_3, _0}
  _6 <- arr21 (-) -< {_1, _5}
  _7 <- arr11 (+1) -< _4
  _8 <- arr21 (+) -< {_6, _4}
  _9 <- pre1 2.661918922156457 -< _4
  _10 <- arr21 (*) -< {_8, _3}
  _11 <- arr21 (*) -< {_7, _10}
  _12 <- arr21 (-) -< {_9, _11}
  _13 <- arr11 (+1) -< _12
  _14 <- arr21 (*) -< {_3, _13}
  _15 <- arr11 (+1) -< _14
  _16 <- arr11 (+1) -< _15
  _17 <- arr11 (+1) -< _16
  _18 <- arr21 (*) -< {_6, _17}
  _19 <- arr21 (*) -< {_18, _4}
  _20 <- arr21 (+) -< {_19, _7}
  _21 <- arr21 (+) -< {_20, _10}
  _22 <- arr21 (-) -< {_8, _2}
  _23 <- arr11 (+1) -< _22
  _24 <- arr21 (+) -< {_22, _23}
  _25 <- arr21 (-) -< {_14, _7}
  _26 <- arr21 (-) -< {_13, _17}
  _27 <- arr21 (*) -< {_0, _25}
  _28 <- pre1 0.6814855198863705 -< _6
  _29 <- arr11 (+1) -< _2
  _30 <- arr11 (+1) -< _24
  _31 <- arr21 (-) -< {_29, _12}
  _32 <- pre1 0.5076694795547356 -< _5
  _33 <- arr21 (+) -< {_27, _21}
  _34 <- arr11 (+1) -< _31
  _35 <- arr21 (*) -< {_28, _33}
  _36 <- arr21 (*) -< {_30, _32}
  _37 <- arr21 (*) -< {_26, _36}
  _38 <- arr21 (-) -< {_35, _37}
  _39 <- pre1 0.2459658880181843 -< _34
  _40 <- arr21 (+) -< {_38, _39}
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0