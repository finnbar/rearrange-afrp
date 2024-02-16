{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
  rec
    _6 <- FRP.Yampa.arr (+1) -< _5
    _7 <- iPre 3.028960234098893 -< _3
    _8 <- iPre 0.7914771179515561 -< _6
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _9)
    _5 <- iPre 1.4762661607265362 -< _4
  _11 <- iPre 2.2929370580811494 -< _6
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
  _13 <- iPre 0.2184314295689059 -< _6
  _14 <- iPre 0.4630749429668599 -< _7
  _15 <- iPre 0.9337806684665964 -< _13
  _16 <- FRP.Yampa.arr (+1) -< _12
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _7)
  _18 <- FRP.Yampa.arr (+1) -< _7
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _1)
  _20 <- iPre 2.039141897984081 -< _16
  _21 <- iPre 1.8937093512337224 -< _17
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _10)
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _11)
  _24 <- iPre 1.494758155869318 -< _14
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _23)
  _26 <- iPre 1.166307255472313 -< _24
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _26)
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- iPre 0.31409289416650343 -< _25
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _28)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  _3 <- arr11 (+1) -< _2
  _4 <- arr21 (-) -< {_1, _3}
  rec
    _6 <- arr11 (+1) -< _5
    _7 <- pre1 3.028960234098893 -< _3
    _8 <- pre1 0.7914771179515561 -< _6
    _9 <- arr11 (+1) -< _8
    _10 <- arr21 (+) -< {_0, _9}
    _5 <- pre1 1.4762661607265362 -< _4

  _11 <- pre1 2.2929370580811494 -< _6
  _12 <- arr21 (+) -< {_2, _2}
  _13 <- pre1 0.2184314295689059 -< _6
  _14 <- pre1 0.4630749429668599 -< _7
  _15 <- pre1 0.9337806684665964 -< _13
  _16 <- arr11 (+1) -< _12
  _17 <- arr21 (*) -< {_15, _7}
  _18 <- arr11 (+1) -< _7
  _19 <- arr21 (+) -< {_18, _1}
  _20 <- pre1 2.039141897984081 -< _16
  _21 <- pre1 1.8937093512337224 -< _17
  _22 <- arr21 (+) -< {_21, _10}
  _23 <- arr21 (*) -< {_22, _11}
  _24 <- pre1 1.494758155869318 -< _14
  _25 <- arr21 (-) -< {_20, _23}
  _26 <- pre1 1.166307255472313 -< _24
  _27 <- arr21 (*) -< {_19, _26}
  _28 <- arr11 (+1) -< _27
  _29 <- pre1 0.31409289416650343 -< _25
  _30 <- arr21 (-) -< {_29, _28}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6