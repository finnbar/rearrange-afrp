{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _2)
    _4 <- FRP.Yampa.arr (+1) -< _2
    _5 <- iPre 0.6453559713072455 -< _2
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _5)
    _1 <- iPre 1.9575228102760645 -< _5
    _2 <- iPre 1.9999285758323946 -< _1
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _4)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _1)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _5)
  _11 <- iPre 3.956831095157971e-2 -< _9
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _3)
  _13 <- iPre 0.945088906720918 -< _8
  _14 <- FRP.Yampa.arr (+1) -< _10
  _15 <- iPre 2.9896465841684066 -< _2
  _16 <- FRP.Yampa.arr (+1) -< _0
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _14)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _13)
  _19 <- FRP.Yampa.arr (+1) -< _10
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _18)
  _21 <- FRP.Yampa.arr (+1) -< _20
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _6)
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _21)
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _23)
  _25 <- iPre 0.6499721525017162 -< _24
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _17)
  _27 <- iPre 3.0033045295599834 -< _26
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _27)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 0.7347315635755934 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr21 (*) -< {_2, _2}
    _4 <- arr11 (+1) -< _2
    _5 <- pre1 0.6453559713072455 -< _2
    _6 <- arr21 (-) -< {_2, _5}
    _1 <- pre1 1.9575228102760645 -< _5
    _2 <- pre1 1.9999285758323946 -< _1

  _7 <- arr21 (*) -< {_1, _1}
  _8 <- arr21 (*) -< {_2, _4}
  _9 <- arr21 (*) -< {_4, _1}
  _10 <- arr21 (-) -< {_7, _5}
  _11 <- pre1 3.956831095157971e-2 -< _9
  _12 <- arr21 (-) -< {_8, _3}
  _13 <- pre1 0.945088906720918 -< _8
  _14 <- arr11 (+1) -< _10
  _15 <- pre1 2.9896465841684066 -< _2
  _16 <- arr11 (+1) -< _0
  _17 <- arr21 (-) -< {_8, _14}
  _18 <- arr21 (+) -< {_12, _13}
  _19 <- arr11 (+1) -< _10
  _20 <- arr21 (+) -< {_11, _18}
  _21 <- arr11 (+1) -< _20
  _22 <- arr21 (+) -< {_19, _6}
  _23 <- arr21 (*) -< {_16, _21}
  _24 <- arr21 (*) -< {_22, _23}
  _25 <- pre1 0.6499721525017162 -< _24
  _26 <- arr21 (-) -< {_25, _17}
  _27 <- pre1 3.0033045295599834 -< _26
  _28 <- arr21 (*) -< {_15, _27}
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 0.7347315635755934 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6