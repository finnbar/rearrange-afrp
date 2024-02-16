{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _4 <- FRP.Yampa.arr (+1) -< _1
  _5 <- iPre 0.3310384280319424 -< _0
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _6)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _1)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _3)
  _10 <- FRP.Yampa.arr (+1) -< _6
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _7)
  _12 <- iPre 2.904493977105306 -< _6
  _13 <- iPre 2.1431269122157515 -< _0
  _14 <- iPre 0.9801625381989357 -< _6
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _0)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _2)
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _2)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _12)
  _19 <- iPre 0.2014909503084406 -< _5
  _20 <- iPre 2.5520965321339286 -< _13
  _21 <- iPre 1.157743524154128 -< _9
  _22 <- FRP.Yampa.arr (+1) -< _20
  _23 <- iPre 2.0575560347117623 -< _16
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _15)
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _18)
  _26 <- iPre 1.8340955966809527 -< _22
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _26)
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _21)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_29, _19)
  _31 <- FRP.Yampa.arr (uncurry (-)) -< (_30, _27)
  _32 <- iPre 1.8614517512361393 -< _31
  _33 <- FRP.Yampa.arr (+1) -< _25
  _34 <- FRP.Yampa.arr (+1) -< _23
  _35 <- FRP.Yampa.arr (uncurry (-)) -< (_32, _34)
  _36 <- iPre 0.6047588905228305 -< _33
  _37 <- iPre 3.04677410555057 -< _36
  _38 <- iPre 0.8662896438226582 -< _37
  _39 <- FRP.Yampa.arr (uncurry (+)) -< (_38, _35)
  _40 <- FRP.Yampa.arr (+1) -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr21 (-) -< {_0, _0}
  _3 <- arr21 (+) -< {_1, _0}
  _4 <- arr11 (+1) -< _1
  _5 <- pre1 0.3310384280319424 -< _0
  _6 <- arr21 (*) -< {_0, _1}
  _7 <- arr21 (-) -< {_4, _6}
  _8 <- arr21 (*) -< {_4, _1}
  _9 <- arr21 (+) -< {_7, _3}
  _10 <- arr11 (+1) -< _6
  _11 <- arr21 (*) -< {_2, _7}
  _12 <- pre1 2.904493977105306 -< _6
  _13 <- pre1 2.1431269122157515 -< _0
  _14 <- pre1 0.9801625381989357 -< _6
  _15 <- arr21 (-) -< {_6, _0}
  _16 <- arr21 (*) -< {_7, _2}
  _17 <- arr21 (*) -< {_14, _2}
  _18 <- arr21 (+) -< {_11, _12}
  _19 <- pre1 0.2014909503084406 -< _5
  _20 <- pre1 2.5520965321339286 -< _13
  _21 <- pre1 1.157743524154128 -< _9
  _22 <- arr11 (+1) -< _20
  _23 <- pre1 2.0575560347117623 -< _16
  _24 <- arr21 (*) -< {_17, _15}
  _25 <- arr21 (*) -< {_24, _18}
  _26 <- pre1 1.8340955966809527 -< _22
  _27 <- arr21 (+) -< {_8, _26}
  _28 <- arr21 (-) -< {_10, _21}
  _29 <- arr11 (+1) -< _28
  _30 <- arr21 (+) -< {_29, _19}
  _31 <- arr21 (-) -< {_30, _27}
  _32 <- pre1 1.8614517512361393 -< _31
  _33 <- arr11 (+1) -< _25
  _34 <- arr11 (+1) -< _23
  _35 <- arr21 (-) -< {_32, _34}
  _36 <- pre1 0.6047588905228305 -< _33
  _37 <- pre1 3.04677410555057 -< _36
  _38 <- pre1 0.8662896438226582 -< _37
  _39 <- arr21 (+) -< {_38, _35}
  _40 <- arr11 (+1) -< _39
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0