{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  rec
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _1)
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _4)
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _5)
    _7 <- iPre 5.755170007296652e-2 -< _0
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _5)
    _9 <- FRP.Yampa.arr (+1) -< _1
    _10 <- FRP.Yampa.arr (+1) -< _7
    _11 <- iPre 0.503563021446492 -< _1
    _2 <- iPre 2.1608352221332607 -< _5
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _3)
  _13 <- iPre 2.7085407712820633 -< _10
  _14 <- iPre 2.4276789083991783 -< _9
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _0)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _6)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _8)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _15)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _17)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  rec
    _3 <- arr21 (+) -< {_2, _1}
    _4 <- arr21 (-) -< {_1, _0}
    _5 <- arr21 (*) -< {_4, _4}
    _6 <- arr21 (+) -< {_0, _5}
    _7 <- pre1 5.755170007296652e-2 -< _0
    _8 <- arr21 (*) -< {_0, _5}
    _9 <- arr11 (+1) -< _1
    _10 <- arr11 (+1) -< _7
    _11 <- pre1 0.503563021446492 -< _1
    _2 <- pre1 2.1608352221332607 -< _5

  _12 <- arr21 (*) -< {_2, _3}
  _13 <- pre1 2.7085407712820633 -< _10
  _14 <- pre1 2.4276789083991783 -< _9
  _15 <- arr21 (-) -< {_12, _0}
  _16 <- arr21 (*) -< {_13, _6}
  _17 <- arr21 (-) -< {_14, _8}
  _18 <- arr21 (*) -< {_16, _15}
  _19 <- arr21 (*) -< {_11, _17}
  _20 <- arr21 (-) -< {_18, _19}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10