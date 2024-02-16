{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
  _4 <- iPre 0.6666306432398995 -< _0
  rec
    _6 <- iPre 0.6476134220273441 -< _5
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _6)
    _8 <- iPre 1.972623458445288 -< _6
    _9 <- FRP.Yampa.arr (+1) -< _7
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _1)
    _11 <- iPre 6.343463667081013e-2 -< _1
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _5 <- iPre 2.58919752493255 -< _5
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _0)
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _3)
  _15 <- iPre 1.6652719597926868 -< _14
  _16 <- iPre 0.9138185297141801 -< _8
  _17 <- FRP.Yampa.arr (+1) -< _11
  _18 <- iPre 1.0623596547392893 -< _10
  _19 <- iPre 0.8145186826945698 -< _18
  _20 <- iPre 0.7739954629726753 -< _15
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _9)
  _22 <- FRP.Yampa.arr (+1) -< _13
  _23 <- FRP.Yampa.arr (+1) -< _20
  _24 <- FRP.Yampa.arr (+1) -< _16
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _4)
  _26 <- FRP.Yampa.arr (+1) -< _23
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _24)
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- FRP.Yampa.arr (+1) -< _25
  _30 <- iPre 0.548603268708861 -< _19
  _31 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _30)
  _32 <- iPre 2.9454819486991908 -< _31
  _33 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _32)
  _34 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _33)
  _35 <- FRP.Yampa.arr (+1) -< _22
  _36 <- iPre 2.623668575923471 -< _35
  _37 <- iPre 1.979809568923133 -< _34
  _38 <- FRP.Yampa.arr (uncurry (-)) -< (_36, _37)
  _39 <- iPre 1.2157904707629748 -< _38
  _40 <- FRP.Yampa.arr (+1) -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (*) -< {_0, _1}
  _3 <- arr21 (+) -< {_0, _1}
  _4 <- pre1 0.6666306432398995 -< _0
  rec
    _6 <- pre1 0.6476134220273441 -< _5
    _7 <- arr21 (+) -< {_0, _6}
    _8 <- pre1 1.972623458445288 -< _6
    _9 <- arr11 (+1) -< _7
    _10 <- arr21 (*) -< {_5, _1}
    _11 <- pre1 6.343463667081013e-2 -< _1
    _12 <- arr21 (*) -< {_0, _0}
    _5 <- pre1 2.58919752493255 -< _5

  _13 <- arr21 (+) -< {_6, _0}
  _14 <- arr21 (-) -< {_12, _3}
  _15 <- pre1 1.6652719597926868 -< _14
  _16 <- pre1 0.9138185297141801 -< _8
  _17 <- arr11 (+1) -< _11
  _18 <- pre1 1.0623596547392893 -< _10
  _19 <- pre1 0.8145186826945698 -< _18
  _20 <- pre1 0.7739954629726753 -< _15
  _21 <- arr21 (+) -< {_17, _9}
  _22 <- arr11 (+1) -< _13
  _23 <- arr11 (+1) -< _20
  _24 <- arr11 (+1) -< _16
  _25 <- arr21 (+) -< {_2, _4}
  _26 <- arr11 (+1) -< _23
  _27 <- arr21 (+) -< {_26, _24}
  _28 <- arr11 (+1) -< _27
  _29 <- arr11 (+1) -< _25
  _30 <- pre1 0.548603268708861 -< _19
  _31 <- arr21 (-) -< {_21, _30}
  _32 <- pre1 2.9454819486991908 -< _31
  _33 <- arr21 (*) -< {_29, _32}
  _34 <- arr21 (*) -< {_28, _33}
  _35 <- arr11 (+1) -< _22
  _36 <- pre1 2.623668575923471 -< _35
  _37 <- pre1 1.979809568923133 -< _34
  _38 <- arr21 (-) -< {_36, _37}
  _39 <- pre1 1.2157904707629748 -< _38
  _40 <- arr11 (+1) -< _39
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8