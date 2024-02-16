{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- iPre 0.7200567021396862 -< _1
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _1)
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _4)
  _6 <- iPre 0.9370882884681976 -< _2
  _7 <- iPre 0.23287752058204192 -< _5
  _8 <- iPre 1.1782702840093011 -< _6
  _9 <- FRP.Yampa.arr (+1) -< _2
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _7)
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _9)
  _12 <- FRP.Yampa.arr (+1) -< _11
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _10)
  _14 <- FRP.Yampa.arr (+1) -< _13
  _15 <- iPre 3.0252387485195844 -< _2
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _15)
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _16)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _10)
  _19 <- iPre 0.6510407248478117 -< _4
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _13)
  _21 <- iPre 2.864323386304525 -< _18
  _22 <- iPre 0.10363603359790112 -< _20
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _21)
  _24 <- iPre 1.9390959541352348 -< _14
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _18)
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _25)
  _27 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _19)
  _28 <- FRP.Yampa.arr (+1) -< _13
  _29 <- iPre 0.4866993512940043 -< _23
  _30 <- FRP.Yampa.arr (+1) -< _13
  _31 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _28)
  _32 <- iPre 1.3356780334719587 -< _27
  _33 <- FRP.Yampa.arr (uncurry (*)) -< (_30, _31)
  _34 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _29)
  _35 <- FRP.Yampa.arr (+1) -< _26
  _36 <- FRP.Yampa.arr (uncurry (*)) -< (_34, _32)
  _37 <- FRP.Yampa.arr (+1) -< _33
  _38 <- FRP.Yampa.arr (uncurry (-)) -< (_36, _35)
  _39 <- FRP.Yampa.arr (+1) -< _38
  _40 <- FRP.Yampa.arr (uncurry (*)) -< (_37, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- pre1 0.7200567021396862 -< _1
  _3 <- arr11 (+1) -< _2
  _4 <- arr21 (+) -< {_3, _1}
  _5 <- arr21 (+) -< {_1, _4}
  _6 <- pre1 0.9370882884681976 -< _2
  _7 <- pre1 0.23287752058204192 -< _5
  _8 <- pre1 1.1782702840093011 -< _6
  _9 <- arr11 (+1) -< _2
  _10 <- arr21 (+) -< {_8, _7}
  _11 <- arr21 (*) -< {_4, _9}
  _12 <- arr11 (+1) -< _11
  _13 <- arr21 (*) -< {_0, _10}
  _14 <- arr11 (+1) -< _13
  _15 <- pre1 3.0252387485195844 -< _2
  _16 <- arr21 (*) -< {_14, _15}
  _17 <- arr21 (*) -< {_12, _16}
  _18 <- arr21 (+) -< {_17, _10}
  _19 <- pre1 0.6510407248478117 -< _4
  _20 <- arr21 (*) -< {_12, _13}
  _21 <- pre1 2.864323386304525 -< _18
  _22 <- pre1 0.10363603359790112 -< _20
  _23 <- arr21 (-) -< {_8, _21}
  _24 <- pre1 1.9390959541352348 -< _14
  _25 <- arr21 (+) -< {_24, _18}
  _26 <- arr21 (*) -< {_16, _25}
  _27 <- arr21 (-) -< {_23, _19}
  _28 <- arr11 (+1) -< _13
  _29 <- pre1 0.4866993512940043 -< _23
  _30 <- arr11 (+1) -< _13
  _31 <- arr21 (-) -< {_0, _28}
  _32 <- pre1 1.3356780334719587 -< _27
  _33 <- arr21 (*) -< {_30, _31}
  _34 <- arr21 (-) -< {_22, _29}
  _35 <- arr11 (+1) -< _26
  _36 <- arr21 (*) -< {_34, _32}
  _37 <- arr11 (+1) -< _33
  _38 <- arr21 (-) -< {_36, _35}
  _39 <- arr11 (+1) -< _38
  _40 <- arr21 (*) -< {_37, _39}
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0