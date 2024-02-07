{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.36085271579151595 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- FRP.Yampa.arr (+1) -< _1
  _4 <- FRP.Yampa.arr (+1) -< _3
  _5 <- FRP.Yampa.arr (+1) -< _1
  rec
    _8 <- FRP.Yampa.arr (+1) -< _1
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _6)
    _10 <- iPre 3.2918487858319674 -< _4
    _11 <- iPre 0.29421047207682705 -< _1
    _6 <- iPre 1.211542378842018 -< _8
    _7 <- iPre 0.7408884314367893 -< _3
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _5)
  _13 <- FRP.Yampa.arr (+1) -< _1
  _14 <- FRP.Yampa.arr (+1) -< _6
  _15 <- iPre 0.404340980196928 -< _12
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _13)
  _17 <- iPre 1.6695155979316134 -< _4
  _18 <- iPre 0.7957436068518069 -< _16
  _19 <- iPre 2.0068135776786944 -< _15
  _20 <- iPre 0.6684829123919783 -< _2
  _21 <- FRP.Yampa.arr (+1) -< _19
  _22 <- iPre 0.7880198235273144 -< _14
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _20)
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _23)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _11)
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _9)
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _25)
  _28 <- FRP.Yampa.arr (+1) -< _26
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _27)
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _28)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.36085271579151595 -< _0
  _2 <- arr11 (+1) -< _1
  _3 <- arr11 (+1) -< _1
  _4 <- arr11 (+1) -< _3
  _5 <- arr11 (+1) -< _1
  rec
    _8 <- arr11 (+1) -< _1
    _9 <- arr21 (-) -< {_7, _6}
    _10 <- pre1 3.2918487858319674 -< _4
    _11 <- pre1 0.29421047207682705 -< _1
    _6 <- pre1 1.211542378842018 -< _8
    _7 <- pre1 0.7408884314367893 -< _3

  _12 <- arr21 (-) -< {_0, _5}
  _13 <- arr11 (+1) -< _1
  _14 <- arr11 (+1) -< _6
  _15 <- pre1 0.404340980196928 -< _12
  _16 <- arr21 (+) -< {_3, _13}
  _17 <- pre1 1.6695155979316134 -< _4
  _18 <- pre1 0.7957436068518069 -< _16
  _19 <- pre1 2.0068135776786944 -< _15
  _20 <- pre1 0.6684829123919783 -< _2
  _21 <- arr11 (+1) -< _19
  _22 <- pre1 0.7880198235273144 -< _14
  _23 <- arr21 (*) -< {_21, _20}
  _24 <- arr21 (+) -< {_18, _23}
  _25 <- arr21 (-) -< {_22, _11}
  _26 <- arr21 (+) -< {_10, _9}
  _27 <- arr21 (*) -< {_17, _25}
  _28 <- arr11 (+1) -< _26
  _29 <- arr21 (+) -< {_24, _27}
  _30 <- arr21 (-) -< {_29, _28}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6