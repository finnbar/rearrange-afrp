{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  rec
    _3 <- iPre 1.9759267835636694 -< _0
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _2)
    _2 <- iPre 2.447866269981818 -< _0
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _5)
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _2)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _1)
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _1)
  _12 <- FRP.Yampa.arr (+1) -< _10
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _9)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _12)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _14)
  _17 <- iPre 1.6027782544177394 -< _8
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _16)
  _19 <- iPre 1.1022282661724805 -< _13
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _18)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  rec
    _3 <- pre1 1.9759267835636694 -< _0
    _4 <- arr21 (*) -< {_0, _0}
    _5 <- arr21 (+) -< {_4, _2}
    _2 <- pre1 2.447866269981818 -< _0

  _6 <- arr21 (-) -< {_2, _3}
  _7 <- arr21 (-) -< {_5, _5}
  _8 <- arr21 (-) -< {_7, _2}
  _9 <- arr21 (+) -< {_0, _1}
  _10 <- arr21 (+) -< {_6, _1}
  _11 <- arr21 (+) -< {_2, _1}
  _12 <- arr11 (+1) -< _10
  _13 <- arr21 (*) -< {_1, _2}
  _14 <- arr21 (+) -< {_1, _9}
  _15 <- arr21 (+) -< {_11, _12}
  _16 <- arr21 (*) -< {_15, _14}
  _17 <- pre1 1.6027782544177394 -< _8
  _18 <- arr21 (-) -< {_17, _16}
  _19 <- pre1 1.1022282661724805 -< _13
  _20 <- arr21 (-) -< {_19, _18}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4