{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- iPre 2.1770166471628873 -< _0
  _3 <- FRP.Yampa.arr (+1) -< _2
  rec
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _0)
    _6 <- FRP.Yampa.arr (+1) -< _2
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _3)
    _8 <- FRP.Yampa.arr (+1) -< _3
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _4)
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _8)
    _11 <- FRP.Yampa.arr (+1) -< _4
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _0)
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _9)
    _4 <- iPre 1.646238219398941 -< _7
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _5)
  _15 <- FRP.Yampa.arr (+1) -< _10
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _12)
  _17 <- iPre 2.4352996261781694 -< _15
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _13)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _18)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- pre1 2.1770166471628873 -< _0
  _3 <- arr11 (+1) -< _2
  rec
    _5 <- arr21 (-) -< {_4, _0}
    _6 <- arr11 (+1) -< _2
    _7 <- arr21 (*) -< {_4, _3}
    _8 <- arr11 (+1) -< _3
    _9 <- arr21 (-) -< {_0, _4}
    _10 <- arr21 (-) -< {_6, _8}
    _11 <- arr11 (+1) -< _4
    _12 <- arr21 (+) -< {_3, _0}
    _13 <- arr21 (-) -< {_4, _9}
    _4 <- pre1 1.646238219398941 -< _7

  _14 <- arr21 (-) -< {_1, _5}
  _15 <- arr11 (+1) -< _10
  _16 <- arr21 (-) -< {_11, _12}
  _17 <- pre1 2.4352996261781694 -< _15
  _18 <- arr21 (+) -< {_14, _13}
  _19 <- arr21 (*) -< {_17, _18}
  _20 <- arr21 (+) -< {_16, _19}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10