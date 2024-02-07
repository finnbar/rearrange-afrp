{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  rec
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _4)
    _2 <- iPre 1.8556070077123552 -< _5
    _3 <- iPre 2.309494852689771 -< _5
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
  _7 <- FRP.Yampa.arr (+1) -< _2
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _3)
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _1)
  _10 <- iPre 1.3713958205639558 -< _4
  _11 <- iPre 2.6137820252680424 -< _10
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _8)
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _11)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _5)
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _6)
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- iPre 2.698629457030176 -< _16
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _15)
  _19 <- iPre 1.286268839694814 -< _18
  _20 <- iPre 0.981062659790556 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  rec
    _4 <- arr21 (+) -< {_0, _2}
    _5 <- arr21 (-) -< {_1, _4}
    _2 <- pre1 1.8556070077123552 -< _5
    _3 <- pre1 2.309494852689771 -< _5

  _6 <- arr21 (+) -< {_0, _1}
  _7 <- arr11 (+1) -< _2
  _8 <- arr21 (+) -< {_7, _3}
  _9 <- arr21 (-) -< {_7, _1}
  _10 <- pre1 1.3713958205639558 -< _4
  _11 <- pre1 2.6137820252680424 -< _10
  _12 <- arr21 (+) -< {_9, _8}
  _13 <- arr21 (-) -< {_0, _11}
  _14 <- arr21 (+) -< {_13, _5}
  _15 <- arr21 (*) -< {_12, _6}
  _16 <- arr11 (+1) -< _14
  _17 <- pre1 2.698629457030176 -< _16
  _18 <- arr21 (-) -< {_17, _15}
  _19 <- pre1 1.286268839694814 -< _18
  _20 <- pre1 0.981062659790556 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4