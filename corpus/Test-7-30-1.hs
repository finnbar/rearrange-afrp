{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- iPre 0.6001883530386575 -< _3
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _4)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _3)
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _5)
    _8 <- FRP.Yampa.arr (+1) -< _0
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- FRP.Yampa.arr (+1) -< _3
    _11 <- iPre 1.6513860701732554 -< _10
    _12 <- FRP.Yampa.arr (+1) -< _1
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _6)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _9)
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _11)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _5)
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _16)
    _18 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _7)
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _14)
    _20 <- iPre 1.2433071249888017 -< _15
    _21 <- iPre 0.5671170040253145 -< _20
    _22 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _21)
    _23 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _12)
    _24 <- FRP.Yampa.arr (+1) -< _18
    _25 <- iPre 3.031914794011486 -< _17
    _26 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _19)
    _27 <- iPre 1.3444732962940906 -< _13
    _28 <- FRP.Yampa.arr (+1) -< _27
    _29 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _22)
    _30 <- iPre 1.7577963842434672 -< _23
    _1 <- iPre 2.6392593983100445 -< _26
    _2 <- iPre 2.9020302998726257 -< _28
    _3 <- iPre 0.8719292419761117 -< _30
  FRP.Yampa.returnA -< _29

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- pre1 0.6001883530386575 -< _3
    _5 <- arr21 (-) -< {_3, _4}
    _6 <- arr21 (*) -< {_3, _3}
    _7 <- arr21 (+) -< {_2, _5}
    _8 <- arr11 (+1) -< _0
    _9 <- arr11 (+1) -< _8
    _10 <- arr11 (+1) -< _3
    _11 <- pre1 1.6513860701732554 -< _10
    _12 <- arr11 (+1) -< _1
    _13 <- arr21 (+) -< {_0, _6}
    _14 <- arr21 (*) -< {_1, _9}
    _15 <- arr21 (+) -< {_10, _11}
    _16 <- arr21 (*) -< {_6, _5}
    _17 <- arr21 (+) -< {_10, _16}
    _18 <- arr21 (-) -< {_15, _7}
    _19 <- arr21 (-) -< {_11, _14}
    _20 <- pre1 1.2433071249888017 -< _15
    _21 <- pre1 0.5671170040253145 -< _20
    _22 <- arr21 (*) -< {_3, _21}
    _23 <- arr21 (+) -< {_16, _12}
    _24 <- arr11 (+1) -< _18
    _25 <- pre1 3.031914794011486 -< _17
    _26 <- arr21 (+) -< {_25, _19}
    _27 <- pre1 1.3444732962940906 -< _13
    _28 <- arr11 (+1) -< _27
    _29 <- arr21 (+) -< {_24, _22}
    _30 <- pre1 1.7577963842434672 -< _23
    _1 <- pre1 2.6392593983100445 -< _26
    _2 <- pre1 2.9020302998726257 -< _28
    _3 <- pre1 0.8719292419761117 -< _30

  GenProc.GeneralisedArrow.returnA -< _29|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30