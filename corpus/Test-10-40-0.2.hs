{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- iPre 0.9190897975697294 -< _1
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- iPre 2.9876924673765477 -< _1
  rec
    _6 <- FRP.Yampa.arr (+1) -< _5
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _6)
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- FRP.Yampa.arr (+1) -< _2
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _11 <- iPre 1.7576599127003147 -< _7
    _12 <- FRP.Yampa.arr (+1) -< _0
    _5 <- iPre 2.7062965197171036 -< _0
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _11)
  _14 <- iPre 0.11791328665884072 -< _12
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _3)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _11)
  _17 <- FRP.Yampa.arr (+1) -< _6
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _11)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _12)
  _20 <- FRP.Yampa.arr (+1) -< _6
  _21 <- FRP.Yampa.arr (+1) -< _3
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _17)
  _23 <- iPre 3.0153355513925355 -< _21
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _7)
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _22)
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _23)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _25)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _13)
  _29 <- iPre 8.742701887421725e-3 -< _4
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _20)
  _31 <- FRP.Yampa.arr (+1) -< _16
  _32 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _27)
  _33 <- FRP.Yampa.arr (+1) -< _30
  _34 <- FRP.Yampa.arr (+1) -< _33
  _35 <- FRP.Yampa.arr (+1) -< _32
  _36 <- FRP.Yampa.arr (uncurry (-)) -< (_35, _29)
  _37 <- iPre 0.24538458362066293 -< _34
  _38 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _37)
  _39 <- FRP.Yampa.arr (uncurry (*)) -< (_36, _31)
  _40 <- FRP.Yampa.arr (uncurry (+)) -< (_38, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- pre1 0.9190897975697294 -< _1
  _3 <- arr11 (+1) -< _2
  _4 <- pre1 2.9876924673765477 -< _1
  rec
    _6 <- arr11 (+1) -< _5
    _7 <- arr21 (*) -< {_5, _6}
    _8 <- arr11 (+1) -< _7
    _9 <- arr11 (+1) -< _2
    _10 <- arr21 (-) -< {_0, _0}
    _11 <- pre1 1.7576599127003147 -< _7
    _12 <- arr11 (+1) -< _0
    _5 <- pre1 2.7062965197171036 -< _0

  _13 <- arr21 (+) -< {_12, _11}
  _14 <- pre1 0.11791328665884072 -< _12
  _15 <- arr21 (+) -< {_5, _3}
  _16 <- arr21 (+) -< {_10, _11}
  _17 <- arr11 (+1) -< _6
  _18 <- arr21 (+) -< {_9, _11}
  _19 <- arr21 (+) -< {_15, _12}
  _20 <- arr11 (+1) -< _6
  _21 <- arr11 (+1) -< _3
  _22 <- arr21 (+) -< {_21, _17}
  _23 <- pre1 3.0153355513925355 -< _21
  _24 <- arr21 (+) -< {_1, _7}
  _25 <- arr21 (+) -< {_19, _22}
  _26 <- arr21 (-) -< {_24, _23}
  _27 <- arr21 (+) -< {_8, _25}
  _28 <- arr21 (*) -< {_14, _13}
  _29 <- pre1 8.742701887421725e-3 -< _4
  _30 <- arr21 (*) -< {_26, _20}
  _31 <- arr11 (+1) -< _16
  _32 <- arr21 (+) -< {_28, _27}
  _33 <- arr11 (+1) -< _30
  _34 <- arr11 (+1) -< _33
  _35 <- arr11 (+1) -< _32
  _36 <- arr21 (-) -< {_35, _29}
  _37 <- pre1 0.24538458362066293 -< _34
  _38 <- arr21 (*) -< {_18, _37}
  _39 <- arr21 (*) -< {_36, _31}
  _40 <- arr21 (+) -< {_38, _39}
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8