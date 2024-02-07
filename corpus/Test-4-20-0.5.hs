{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
  rec
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _3)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _3)
    _9 <- iPre 0.7309768060260513 -< _7
    _10 <- iPre 1.6605246189776688 -< _8
    _11 <- FRP.Yampa.arr (+1) -< _4
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _9)
    _13 <- FRP.Yampa.arr (+1) -< _1
    _4 <- iPre 0.49696567039580414 -< _2
    _5 <- iPre 1.0849676795544057 -< _11
    _6 <- iPre 1.9484107096161232 -< _5
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _13)
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _12)
  _16 <- FRP.Yampa.arr (+1) -< _9
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _16)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _10)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _18)
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (+) -< {_0, _1}
  _3 <- arr21 (*) -< {_2, _1}
  rec
    _7 <- arr21 (-) -< {_4, _3}
    _8 <- arr21 (*) -< {_6, _3}
    _9 <- pre1 0.7309768060260513 -< _7
    _10 <- pre1 1.6605246189776688 -< _8
    _11 <- arr11 (+1) -< _4
    _12 <- arr21 (+) -< {_6, _9}
    _13 <- arr11 (+1) -< _1
    _4 <- pre1 0.49696567039580414 -< _2
    _5 <- pre1 1.0849676795544057 -< _11
    _6 <- pre1 1.9484107096161232 -< _5

  _14 <- arr21 (*) -< {_7, _13}
  _15 <- arr21 (-) -< {_14, _12}
  _16 <- arr11 (+1) -< _9
  _17 <- arr21 (-) -< {_9, _16}
  _18 <- arr21 (+) -< {_15, _10}
  _19 <- arr21 (-) -< {_17, _18}
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10