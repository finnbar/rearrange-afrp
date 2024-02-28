{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
    _3 <- FRP.Yampa.arr (+1) -< _2
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _0)
    _5 <- iPre 2.804153156452843 -< _4
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _5)
    _7 <- iPre 0.7835249784188467 -< _2
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _8)
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _6)
    _11 <- iPre 9.71346760150082e-2 -< _10
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _10)
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _12)
    _14 <- iPre 0.19976350131938403 -< _13
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _9)
    _16 <- FRP.Yampa.arr (+1) -< _14
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _4)
    _18 <- iPre 0.3486483436176 -< _16
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _18)
    _20 <- FRP.Yampa.arr (+1) -< _19
    _1 <- iPre 1.3203872968480308 -< _17
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_1, _0}
    _3 <- arr11 (+1) -< _2
    _4 <- arr21 (*) -< {_3, _0}
    _5 <- pre1 2.804153156452843 -< _4
    _6 <- arr21 (-) -< {_4, _5}
    _7 <- pre1 0.7835249784188467 -< _2
    _8 <- arr11 (+1) -< _7
    _9 <- arr21 (-) -< {_4, _8}
    _10 <- arr21 (-) -< {_8, _6}
    _11 <- pre1 9.71346760150082e-2 -< _10
    _12 <- arr21 (-) -< {_11, _10}
    _13 <- arr21 (*) -< {_9, _12}
    _14 <- pre1 0.19976350131938403 -< _13
    _15 <- arr21 (+) -< {_5, _9}
    _16 <- arr11 (+1) -< _14
    _17 <- arr21 (*) -< {_15, _4}
    _18 <- pre1 0.3486483436176 -< _16
    _19 <- arr21 (-) -< {_4, _18}
    _20 <- arr11 (+1) -< _19
    _1 <- pre1 1.3203872968480308 -< _17

  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20