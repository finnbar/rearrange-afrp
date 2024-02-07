{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _4 <- iPre 0.46336421724228477 -< _3
  rec
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _1)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _3)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _7)
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _6)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _11)
    _13 <- iPre 0.5787193209768696 -< _5
    _14 <- iPre 2.6899477691594034 -< _6
    _15 <- iPre 1.705152002549601 -< _13
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _2)
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _7)
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _10)
    _19 <- iPre 1.6373349492818108 -< _7
    _5 <- iPre 1.1913175737117148 -< _19
    _6 <- iPre 0.47300472696675444 -< _16
    _7 <- iPre 2.0661643985335156 -< _17
  _20 <- FRP.Yampa.arr (+1) -< _16
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _18)
  _22 <- iPre 1.567225725992569 -< _15
  _23 <- iPre 1.5913611219337789 -< _14
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _22)
  _25 <- FRP.Yampa.arr (+1) -< _23
  _26 <- iPre 1.1316185695429732 -< _24
  _27 <- iPre 2.1896456741168793 -< _25
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _26)
  _30 <- iPre 1.2573566897709494 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  _3 <- arr21 (*) -< {_0, _1}
  _4 <- pre1 0.46336421724228477 -< _3
  rec
    _8 <- arr21 (+) -< {_7, _1}
    _9 <- arr21 (-) -< {_3, _3}
    _10 <- arr21 (+) -< {_8, _7}
    _11 <- arr21 (-) -< {_9, _6}
    _12 <- arr21 (*) -< {_7, _11}
    _13 <- pre1 0.5787193209768696 -< _5
    _14 <- pre1 2.6899477691594034 -< _6
    _15 <- pre1 1.705152002549601 -< _13
    _16 <- arr21 (+) -< {_4, _2}
    _17 <- arr21 (+) -< {_12, _7}
    _18 <- arr21 (*) -< {_5, _10}
    _19 <- pre1 1.6373349492818108 -< _7
    _5 <- pre1 1.1913175737117148 -< _19
    _6 <- pre1 0.47300472696675444 -< _16
    _7 <- pre1 2.0661643985335156 -< _17

  _20 <- arr11 (+1) -< _16
  _21 <- arr21 (-) -< {_20, _18}
  _22 <- pre1 1.567225725992569 -< _15
  _23 <- pre1 1.5913611219337789 -< _14
  _24 <- arr21 (-) -< {_21, _22}
  _25 <- arr11 (+1) -< _23
  _26 <- pre1 1.1316185695429732 -< _24
  _27 <- pre1 2.1896456741168793 -< _25
  _28 <- arr11 (+1) -< _27
  _29 <- arr21 (*) -< {_28, _26}
  _30 <- pre1 1.2573566897709494 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15