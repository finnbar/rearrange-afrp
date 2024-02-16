{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- iPre 2.677808575479157 -< _1
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _2)
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _0)
  _6 <- iPre 2.5555283010996983 -< _2
  _7 <- iPre 0.1402762425221706 -< _3
  _8 <- FRP.Yampa.arr (+1) -< _5
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _2)
  _10 <- iPre 2.6426426586489824 -< _1
  _11 <- FRP.Yampa.arr (+1) -< _8
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _8)
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _10)
  _14 <- iPre 2.4332333188493287 -< _2
  _15 <- iPre 1.69245778279064 -< _14
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _9)
  _17 <- iPre 1.115829228062309 -< _10
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _11)
  _19 <- FRP.Yampa.arr (+1) -< _18
  _20 <- iPre 1.8145740957552545 -< _7
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _12)
  _22 <- iPre 9.810650526959235e-2 -< _7
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _13)
  _24 <- iPre 1.798755081839932 -< _11
  _25 <- iPre 2.5104979589050953 -< _24
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _15)
  _27 <- FRP.Yampa.arr (+1) -< _17
  _28 <- iPre 3.0008317066323036 -< _21
  _29 <- FRP.Yampa.arr (+1) -< _20
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _25)
  _31 <- iPre 1.5999970357686057 -< _29
  _32 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _30)
  _33 <- FRP.Yampa.arr (+1) -< _22
  _34 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _27)
  _35 <- FRP.Yampa.arr (uncurry (-)) -< (_32, _26)
  _36 <- FRP.Yampa.arr (+1) -< _31
  _37 <- FRP.Yampa.arr (uncurry (+)) -< (_35, _36)
  _38 <- FRP.Yampa.arr (uncurry (+)) -< (_33, _34)
  _39 <- FRP.Yampa.arr (+1) -< _37
  _40 <- FRP.Yampa.arr (uncurry (-)) -< (_39, _38)
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- pre1 2.677808575479157 -< _1
  _4 <- arr21 (*) -< {_0, _2}
  _5 <- arr21 (-) -< {_2, _0}
  _6 <- pre1 2.5555283010996983 -< _2
  _7 <- pre1 0.1402762425221706 -< _3
  _8 <- arr11 (+1) -< _5
  _9 <- arr21 (+) -< {_4, _2}
  _10 <- pre1 2.6426426586489824 -< _1
  _11 <- arr11 (+1) -< _8
  _12 <- arr21 (*) -< {_8, _8}
  _13 <- arr21 (+) -< {_3, _10}
  _14 <- pre1 2.4332333188493287 -< _2
  _15 <- pre1 1.69245778279064 -< _14
  _16 <- arr21 (*) -< {_10, _9}
  _17 <- pre1 1.115829228062309 -< _10
  _18 <- arr21 (-) -< {_1, _11}
  _19 <- arr11 (+1) -< _18
  _20 <- pre1 1.8145740957552545 -< _7
  _21 <- arr21 (+) -< {_19, _12}
  _22 <- pre1 9.810650526959235e-2 -< _7
  _23 <- arr21 (*) -< {_2, _13}
  _24 <- pre1 1.798755081839932 -< _11
  _25 <- pre1 2.5104979589050953 -< _24
  _26 <- arr21 (*) -< {_23, _15}
  _27 <- arr11 (+1) -< _17
  _28 <- pre1 3.0008317066323036 -< _21
  _29 <- arr11 (+1) -< _20
  _30 <- arr21 (+) -< {_16, _25}
  _31 <- pre1 1.5999970357686057 -< _29
  _32 <- arr21 (*) -< {_6, _30}
  _33 <- arr11 (+1) -< _22
  _34 <- arr21 (*) -< {_28, _27}
  _35 <- arr21 (-) -< {_32, _26}
  _36 <- arr11 (+1) -< _31
  _37 <- arr21 (+) -< {_35, _36}
  _38 <- arr21 (+) -< {_33, _34}
  _39 <- arr11 (+1) -< _37
  _40 <- arr21 (-) -< {_39, _38}
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0