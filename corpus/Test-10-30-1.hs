{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _4 <- iPre 2.923103760890375 -< _0
    _5 <- iPre 1.1483159368868705 -< _4
    _6 <- FRP.Yampa.arr (+1) -< _1
    _7 <- iPre 2.0466505719313206 -< _6
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- iPre 1.1233217404429263 -< _1
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _1)
    _11 <- iPre 1.066973502752121 -< _10
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _3)
    _13 <- FRP.Yampa.arr (+1) -< _5
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _2)
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _13)
    _16 <- FRP.Yampa.arr (+1) -< _15
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _16)
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _12)
    _19 <- FRP.Yampa.arr (+1) -< _18
    _20 <- FRP.Yampa.arr (+1) -< _14
    _21 <- iPre 1.7327509651559407 -< _6
    _22 <- iPre 2.661845702262499 -< _13
    _23 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _17)
    _24 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _23)
    _25 <- FRP.Yampa.arr (+1) -< _22
    _26 <- FRP.Yampa.arr (+1) -< _21
    _27 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _19)
    _28 <- FRP.Yampa.arr (+1) -< _27
    _29 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _25)
    _30 <- FRP.Yampa.arr (+1) -< _29
    _1 <- iPre 1.821942427301051 -< _30
  FRP.Yampa.returnA -< _24

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_0, _0}
    _3 <- arr21 (+) -< {_0, _1}
    _4 <- pre1 2.923103760890375 -< _0
    _5 <- pre1 1.1483159368868705 -< _4
    _6 <- arr11 (+1) -< _1
    _7 <- pre1 2.0466505719313206 -< _6
    _8 <- arr11 (+1) -< _7
    _9 <- pre1 1.1233217404429263 -< _1
    _10 <- arr21 (-) -< {_9, _1}
    _11 <- pre1 1.066973502752121 -< _10
    _12 <- arr21 (*) -< {_7, _3}
    _13 <- arr11 (+1) -< _5
    _14 <- arr21 (*) -< {_8, _2}
    _15 <- arr21 (*) -< {_14, _13}
    _16 <- arr11 (+1) -< _15
    _17 <- arr21 (*) -< {_16, _16}
    _18 <- arr21 (*) -< {_11, _12}
    _19 <- arr11 (+1) -< _18
    _20 <- arr11 (+1) -< _14
    _21 <- pre1 1.7327509651559407 -< _6
    _22 <- pre1 2.661845702262499 -< _13
    _23 <- arr21 (-) -< {_20, _17}
    _24 <- arr21 (-) -< {_0, _23}
    _25 <- arr11 (+1) -< _22
    _26 <- arr11 (+1) -< _21
    _27 <- arr21 (*) -< {_26, _19}
    _28 <- arr11 (+1) -< _27
    _29 <- arr21 (-) -< {_28, _25}
    _30 <- arr11 (+1) -< _29
    _1 <- pre1 1.821942427301051 -< _30

  GenProc.GeneralisedArrow.returnA -< _24|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30