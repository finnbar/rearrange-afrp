{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
    _3 <- iPre 2.632662994798767 -< _1
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
    _6 <- iPre 0.6439782143585849 -< _0
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _1)
    _8 <- FRP.Yampa.arr (+1) -< _0
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _4)
    _10 <- iPre 1.6508432695673487 -< _4
    _11 <- iPre 2.198994999861991 -< _0
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _9)
    _13 <- FRP.Yampa.arr (+1) -< _7
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _5)
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _8)
    _16 <- iPre 0.6394185304831616 -< _15
    _17 <- iPre 1.5734013168504424 -< _15
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _9)
    _19 <- FRP.Yampa.arr (+1) -< _12
    _20 <- iPre 0.9820115458721862 -< _14
    _21 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _3)
    _22 <- iPre 0.497998905847534 -< _11
    _23 <- FRP.Yampa.arr (+1) -< _22
    _24 <- iPre 0.40335637197877383 -< _16
    _25 <- FRP.Yampa.arr (+1) -< _18
    _26 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _10)
    _27 <- FRP.Yampa.arr (+1) -< _2
    _28 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _24)
    _29 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _17)
    _30 <- iPre 1.7246043075245046 -< _20
    _31 <- iPre 1.2972532636068594 -< _28
    _32 <- FRP.Yampa.arr (+1) -< _27
    _33 <- FRP.Yampa.arr (+1) -< _30
    _34 <- FRP.Yampa.arr (uncurry (-)) -< (_33, _32)
    _35 <- FRP.Yampa.arr (+1) -< _26
    _36 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _34)
    _37 <- FRP.Yampa.arr (uncurry (+)) -< (_31, _23)
    _38 <- FRP.Yampa.arr (uncurry (-)) -< (_36, _35)
    _39 <- FRP.Yampa.arr (+1) -< _37
    _40 <- iPre 2.0310441821871446 -< _38
    _1 <- iPre 0.8157899294434762 -< _40
  FRP.Yampa.returnA -< _39

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_1, _1}
    _3 <- pre1 2.632662994798767 -< _1
    _4 <- arr11 (+1) -< _1
    _5 <- arr21 (-) -< {_1, _1}
    _6 <- pre1 0.6439782143585849 -< _0
    _7 <- arr21 (-) -< {_6, _1}
    _8 <- arr11 (+1) -< _0
    _9 <- arr21 (-) -< {_0, _4}
    _10 <- pre1 1.6508432695673487 -< _4
    _11 <- pre1 2.198994999861991 -< _0
    _12 <- arr21 (-) -< {_5, _9}
    _13 <- arr11 (+1) -< _7
    _14 <- arr21 (+) -< {_0, _5}
    _15 <- arr21 (+) -< {_12, _8}
    _16 <- pre1 0.6394185304831616 -< _15
    _17 <- pre1 1.5734013168504424 -< _15
    _18 <- arr21 (+) -< {_13, _9}
    _19 <- arr11 (+1) -< _12
    _20 <- pre1 0.9820115458721862 -< _14
    _21 <- arr21 (-) -< {_3, _3}
    _22 <- pre1 0.497998905847534 -< _11
    _23 <- arr11 (+1) -< _22
    _24 <- pre1 0.40335637197877383 -< _16
    _25 <- arr11 (+1) -< _18
    _26 <- arr21 (-) -< {_25, _10}
    _27 <- arr11 (+1) -< _2
    _28 <- arr21 (-) -< {_19, _24}
    _29 <- arr21 (-) -< {_21, _17}
    _30 <- pre1 1.7246043075245046 -< _20
    _31 <- pre1 1.2972532636068594 -< _28
    _32 <- arr11 (+1) -< _27
    _33 <- arr11 (+1) -< _30
    _34 <- arr21 (-) -< {_33, _32}
    _35 <- arr11 (+1) -< _26
    _36 <- arr21 (*) -< {_29, _34}
    _37 <- arr21 (+) -< {_31, _23}
    _38 <- arr21 (-) -< {_36, _35}
    _39 <- arr11 (+1) -< _37
    _40 <- pre1 2.0310441821871446 -< _38
    _1 <- pre1 0.8157899294434762 -< _40

  GenProc.GeneralisedArrow.returnA -< _39|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40