{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _2)
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
  _6 <- FRP.Yampa.arr (+1) -< _5
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _6)
  _8 <- iPre 0.963623496466395 -< _7
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _7)
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _9)
  _12 <- iPre 0.74895077483059 -< _4
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _11)
  _14 <- iPre 1.5907265452340689 -< _7
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _14)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _14)
  _17 <- iPre 2.328371292150253 -< _12
  _18 <- iPre 0.8609775232564127 -< _1
  _19 <- iPre 1.906504527994905 -< _1
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _16)
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _7)
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _13)
  _23 <- FRP.Yampa.arr (+1) -< _2
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _15)
  _25 <- iPre 1.514738628324979 -< _24
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _23)
  _27 <- FRP.Yampa.arr (+1) -< _21
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _18)
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _26)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_27, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  _3 <- arr11 (+1) -< _2
  _4 <- arr21 (-) -< {_3, _2}
  _5 <- arr21 (+) -< {_4, _1}
  _6 <- arr11 (+1) -< _5
  _7 <- arr21 (+) -< {_3, _6}
  _8 <- pre1 0.963623496466395 -< _7
  _9 <- arr11 (+1) -< _8
  _10 <- arr21 (+) -< {_9, _7}
  _11 <- arr21 (*) -< {_10, _9}
  _12 <- pre1 0.74895077483059 -< _4
  _13 <- arr21 (-) -< {_8, _11}
  _14 <- pre1 1.5907265452340689 -< _7
  _15 <- arr21 (*) -< {_5, _14}
  _16 <- arr21 (*) -< {_0, _14}
  _17 <- pre1 2.328371292150253 -< _12
  _18 <- pre1 0.8609775232564127 -< _1
  _19 <- pre1 1.906504527994905 -< _1
  _20 <- arr21 (-) -< {_10, _16}
  _21 <- arr21 (*) -< {_19, _7}
  _22 <- arr21 (+) -< {_17, _13}
  _23 <- arr11 (+1) -< _2
  _24 <- arr21 (-) -< {_20, _15}
  _25 <- pre1 1.514738628324979 -< _24
  _26 <- arr21 (+) -< {_25, _23}
  _27 <- arr11 (+1) -< _21
  _28 <- arr21 (-) -< {_22, _18}
  _29 <- arr21 (-) -< {_28, _26}
  _30 <- arr21 (-) -< {_27, _29}
  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0