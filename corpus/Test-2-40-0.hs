{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.5112384388956164 -< _0
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  _3 <- iPre 8.999109586960226e-2 -< _1
  _4 <- iPre 1.573722326484009 -< _1
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _5)
  _7 <- iPre 2.2462098566687723 -< _2
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _8)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _7)
  _11 <- FRP.Yampa.arr (+1) -< _7
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _10)
  _13 <- iPre 0.592404069300291 -< _5
  _14 <- iPre 0.8783985598560299 -< _2
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _8)
  _16 <- iPre 1.5549352133364602 -< _13
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _12)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _9)
  _19 <- iPre 2.6970416447385737 -< _11
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _17)
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _6)
  _22 <- FRP.Yampa.arr (+1) -< _19
  _23 <- FRP.Yampa.arr (+1) -< _15
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _22)
  _25 <- iPre 1.0977391390238693 -< _20
  _26 <- FRP.Yampa.arr (+1) -< _25
  _27 <- FRP.Yampa.arr (+1) -< _23
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- FRP.Yampa.arr (+1) -< _24
  _30 <- FRP.Yampa.arr (+1) -< _26
  _31 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _4)
  _32 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _18)
  _33 <- FRP.Yampa.arr (uncurry (-)) -< (_32, _31)
  _34 <- FRP.Yampa.arr (uncurry (-)) -< (_33, _30)
  _35 <- FRP.Yampa.arr (+1) -< _34
  _36 <- FRP.Yampa.arr (+1) -< _35
  _37 <- FRP.Yampa.arr (+1) -< _36
  _38 <- iPre 0.423912720223362 -< _37
  _39 <- iPre 2.5270336638769457 -< _38
  _40 <- iPre 0.5710004067984389 -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.5112384388956164 -< _0
  _2 <- arr21 (+) -< {_1, _1}
  _3 <- pre1 8.999109586960226e-2 -< _1
  _4 <- pre1 1.573722326484009 -< _1
  _5 <- arr21 (*) -< {_1, _0}
  _6 <- arr21 (-) -< {_2, _5}
  _7 <- pre1 2.2462098566687723 -< _2
  _8 <- arr21 (+) -< {_1, _1}
  _9 <- arr21 (*) -< {_7, _8}
  _10 <- arr21 (*) -< {_5, _7}
  _11 <- arr11 (+1) -< _7
  _12 <- arr21 (*) -< {_7, _10}
  _13 <- pre1 0.592404069300291 -< _5
  _14 <- pre1 0.8783985598560299 -< _2
  _15 <- arr21 (*) -< {_3, _8}
  _16 <- pre1 1.5549352133364602 -< _13
  _17 <- arr21 (+) -< {_10, _12}
  _18 <- arr21 (+) -< {_14, _9}
  _19 <- pre1 2.6970416447385737 -< _11
  _20 <- arr21 (-) -< {_11, _17}
  _21 <- arr21 (*) -< {_16, _6}
  _22 <- arr11 (+1) -< _19
  _23 <- arr11 (+1) -< _15
  _24 <- arr21 (*) -< {_21, _22}
  _25 <- pre1 1.0977391390238693 -< _20
  _26 <- arr11 (+1) -< _25
  _27 <- arr11 (+1) -< _23
  _28 <- arr11 (+1) -< _27
  _29 <- arr11 (+1) -< _24
  _30 <- arr11 (+1) -< _26
  _31 <- arr21 (-) -< {_28, _4}
  _32 <- arr21 (*) -< {_29, _18}
  _33 <- arr21 (-) -< {_32, _31}
  _34 <- arr21 (-) -< {_33, _30}
  _35 <- arr11 (+1) -< _34
  _36 <- arr11 (+1) -< _35
  _37 <- arr11 (+1) -< _36
  _38 <- pre1 0.423912720223362 -< _37
  _39 <- pre1 2.5270336638769457 -< _38
  _40 <- pre1 0.5710004067984389 -< _39
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0