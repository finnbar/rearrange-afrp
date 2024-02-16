{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.6905588459457683 -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
  rec
    _5 <- iPre 3.063232411019699 -< _0
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _0)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _6)
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
    _11 <- FRP.Yampa.arr (+1) -< _9
    _12 <- FRP.Yampa.arr (+1) -< _10
    _13 <- iPre 1.6306236188118914 -< _12
    _14 <- FRP.Yampa.arr (+1) -< _10
    _15 <- iPre 0.13094080211928777 -< _8
    _16 <- FRP.Yampa.arr (+1) -< _11
    _17 <- iPre 1.2443616006581752 -< _9
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _14)
    _4 <- iPre 2.9621063185756196 -< _6
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _4)
  _20 <- FRP.Yampa.arr (+1) -< _15
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
  _22 <- iPre 0.14203115942005623 -< _21
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _0)
  _24 <- FRP.Yampa.arr (+1) -< _18
  _25 <- iPre 1.7330617629170397 -< _19
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _25)
  _27 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _24)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _26)
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _23)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _28)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.6905588459457683 -< _0
  _2 <- arr21 (-) -< {_0, _1}
  _3 <- arr21 (-) -< {_2, _1}
  rec
    _5 <- pre1 3.063232411019699 -< _0
    _6 <- arr21 (+) -< {_3, _0}
    _7 <- arr21 (-) -< {_5, _6}
    _8 <- arr11 (+1) -< _7
    _9 <- arr11 (+1) -< _6
    _10 <- arr21 (+) -< {_4, _1}
    _11 <- arr11 (+1) -< _9
    _12 <- arr11 (+1) -< _10
    _13 <- pre1 1.6306236188118914 -< _12
    _14 <- arr11 (+1) -< _10
    _15 <- pre1 0.13094080211928777 -< _8
    _16 <- arr11 (+1) -< _11
    _17 <- pre1 1.2443616006581752 -< _9
    _18 <- arr21 (*) -< {_17, _14}
    _4 <- pre1 2.9621063185756196 -< _6

  _19 <- arr21 (*) -< {_11, _4}
  _20 <- arr11 (+1) -< _15
  _21 <- arr21 (+) -< {_2, _2}
  _22 <- pre1 0.14203115942005623 -< _21
  _23 <- arr21 (*) -< {_13, _0}
  _24 <- arr11 (+1) -< _18
  _25 <- pre1 1.7330617629170397 -< _19
  _26 <- arr21 (+) -< {_20, _25}
  _27 <- arr21 (-) -< {_16, _24}
  _28 <- arr21 (*) -< {_27, _26}
  _29 <- arr21 (+) -< {_22, _23}
  _30 <- arr21 (*) -< {_29, _28}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15