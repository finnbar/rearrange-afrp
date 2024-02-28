{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _2)
    _5 <- iPre 0.8005930057430996 -< _1
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _2)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _6)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _5)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _8)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _8)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _7)
    _12 <- FRP.Yampa.arr (+1) -< _4
    _13 <- iPre 2.88260959396985 -< _2
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _4)
    _15 <- FRP.Yampa.arr (+1) -< _0
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _15)
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _11)
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _16)
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _18)
    _20 <- iPre 1.1793799551408357 -< _19
    _1 <- iPre 0.4566715528826421 -< _14
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_0, _1}
    _3 <- arr21 (*) -< {_1, _2}
    _4 <- arr21 (*) -< {_2, _2}
    _5 <- pre1 0.8005930057430996 -< _1
    _6 <- arr21 (*) -< {_2, _2}
    _7 <- arr21 (-) -< {_5, _6}
    _8 <- arr21 (+) -< {_1, _5}
    _9 <- arr21 (-) -< {_3, _8}
    _10 <- arr21 (+) -< {_9, _8}
    _11 <- arr21 (*) -< {_4, _7}
    _12 <- arr11 (+1) -< _4
    _13 <- pre1 2.88260959396985 -< _2
    _14 <- arr21 (*) -< {_1, _4}
    _15 <- arr11 (+1) -< _0
    _16 <- arr21 (+) -< {_13, _15}
    _17 <- arr21 (+) -< {_10, _11}
    _18 <- arr21 (+) -< {_12, _16}
    _19 <- arr21 (-) -< {_17, _18}
    _20 <- pre1 1.1793799551408357 -< _19
    _1 <- pre1 0.4566715528826421 -< _14

  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20