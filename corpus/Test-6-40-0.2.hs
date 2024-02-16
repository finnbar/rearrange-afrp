{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  rec
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _1)
    _7 <- FRP.Yampa.arr (+1) -< _1
    _8 <- FRP.Yampa.arr (+1) -< _1
    _9 <- FRP.Yampa.arr (+1) -< _4
    _2 <- iPre 0.7487939005375684 -< _9
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _3)
  _11 <- FRP.Yampa.arr (+1) -< _6
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _6)
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _12)
  _14 <- iPre 2.260272453973732 -< _6
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _5)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _11)
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _10)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _16)
  _19 <- FRP.Yampa.arr (+1) -< _17
  _20 <- iPre 1.0755162721684919 -< _15
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _19)
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _11)
  _23 <- iPre 0.1890577272944314 -< _15
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _13)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _14)
  _26 <- FRP.Yampa.arr (+1) -< _20
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _26)
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _22)
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _21)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _29)
  _31 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _28)
  _32 <- FRP.Yampa.arr (uncurry (*)) -< (_30, _27)
  _33 <- iPre 0.16098566423122185 -< _18
  _34 <- FRP.Yampa.arr (+1) -< _31
  _35 <- iPre 0.6924647920398554 -< _32
  _36 <- FRP.Yampa.arr (+1) -< _33
  _37 <- FRP.Yampa.arr (+1) -< _35
  _38 <- FRP.Yampa.arr (uncurry (+)) -< (_34, _36)
  _39 <- FRP.Yampa.arr (uncurry (+)) -< (_38, _23)
  _40 <- FRP.Yampa.arr (uncurry (-)) -< (_39, _37)
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  rec
    _3 <- arr21 (-) -< {_0, _0}
    _4 <- arr21 (-) -< {_0, _0}
    _5 <- arr21 (+) -< {_2, _2}
    _6 <- arr21 (-) -< {_4, _1}
    _7 <- arr11 (+1) -< _1
    _8 <- arr11 (+1) -< _1
    _9 <- arr11 (+1) -< _4
    _2 <- pre1 0.7487939005375684 -< _9

  _10 <- arr21 (*) -< {_7, _3}
  _11 <- arr11 (+1) -< _6
  _12 <- arr21 (+) -< {_8, _6}
  _13 <- arr21 (+) -< {_6, _12}
  _14 <- pre1 2.260272453973732 -< _6
  _15 <- arr21 (+) -< {_7, _5}
  _16 <- arr21 (*) -< {_14, _11}
  _17 <- arr21 (*) -< {_10, _10}
  _18 <- arr21 (+) -< {_10, _16}
  _19 <- arr11 (+1) -< _17
  _20 <- pre1 1.0755162721684919 -< _15
  _21 <- arr21 (-) -< {_9, _19}
  _22 <- arr21 (+) -< {_15, _11}
  _23 <- pre1 0.1890577272944314 -< _15
  _24 <- arr21 (+) -< {_13, _13}
  _25 <- arr21 (-) -< {_0, _14}
  _26 <- arr11 (+1) -< _20
  _27 <- arr21 (*) -< {_16, _26}
  _28 <- arr21 (+) -< {_25, _22}
  _29 <- arr21 (+) -< {_17, _21}
  _30 <- arr21 (*) -< {_24, _29}
  _31 <- arr21 (+) -< {_2, _28}
  _32 <- arr21 (*) -< {_30, _27}
  _33 <- pre1 0.16098566423122185 -< _18
  _34 <- arr11 (+1) -< _31
  _35 <- pre1 0.6924647920398554 -< _32
  _36 <- arr11 (+1) -< _33
  _37 <- arr11 (+1) -< _35
  _38 <- arr21 (+) -< {_34, _36}
  _39 <- arr21 (+) -< {_38, _23}
  _40 <- arr21 (-) -< {_39, _37}
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8