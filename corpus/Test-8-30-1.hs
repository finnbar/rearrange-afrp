{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _4)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _6)
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
    _11 <- iPre 0.38785487873392727 -< _6
    _12 <- iPre 1.4696581686175987 -< _2
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _2)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _9)
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _2)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _8)
    _17 <- iPre 2.759298116299411 -< _7
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _12)
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _12)
    _20 <- FRP.Yampa.arr (+1) -< _19
    _21 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _10)
    _22 <- FRP.Yampa.arr (+1) -< _6
    _23 <- iPre 0.6259076252882141 -< _18
    _24 <- FRP.Yampa.arr (+1) -< _23
    _25 <- FRP.Yampa.arr (+1) -< _14
    _26 <- iPre 1.968807771581624 -< _24
    _27 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _22)
    _28 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _5)
    _29 <- FRP.Yampa.arr (+1) -< _21
    _30 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _26)
    _1 <- iPre 2.876900979239961 -< _27
    _2 <- iPre 0.13036925657230905 -< _17
    _3 <- iPre 0.11095929946057596 -< _28
    _4 <- iPre 1.0629061967286735 -< _30
    _5 <- iPre 1.6609631881136855 -< _29
    _6 <- iPre 0.651810923173351 -< _15
  FRP.Yampa.returnA -< _25

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _7 <- arr21 (-) -< {_0, _4}
    _8 <- arr21 (+) -< {_0, _0}
    _9 <- arr21 (+) -< {_6, _6}
    _10 <- arr21 (-) -< {_0, _2}
    _11 <- pre1 0.38785487873392727 -< _6
    _12 <- pre1 1.4696581686175987 -< _2
    _13 <- arr21 (-) -< {_7, _2}
    _14 <- arr21 (+) -< {_4, _9}
    _15 <- arr21 (+) -< {_10, _2}
    _16 <- arr21 (*) -< {_1, _8}
    _17 <- pre1 2.759298116299411 -< _7
    _18 <- arr21 (+) -< {_11, _12}
    _19 <- arr21 (+) -< {_3, _12}
    _20 <- arr11 (+1) -< _19
    _21 <- arr21 (*) -< {_20, _10}
    _22 <- arr11 (+1) -< _6
    _23 <- pre1 0.6259076252882141 -< _18
    _24 <- arr11 (+1) -< _23
    _25 <- arr11 (+1) -< _14
    _26 <- pre1 1.968807771581624 -< _24
    _27 <- arr21 (+) -< {_16, _22}
    _28 <- arr21 (+) -< {_12, _5}
    _29 <- arr11 (+1) -< _21
    _30 <- arr21 (-) -< {_13, _26}
    _1 <- pre1 2.876900979239961 -< _27
    _2 <- pre1 0.13036925657230905 -< _17
    _3 <- pre1 0.11095929946057596 -< _28
    _4 <- pre1 1.0629061967286735 -< _30
    _5 <- pre1 1.6609631881136855 -< _29
    _6 <- pre1 0.651810923173351 -< _15

  GenProc.GeneralisedArrow.returnA -< _25|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30