{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- iPre 1.3489706223561213 -< _1
  rec
    _5 <- FRP.Yampa.arr (+1) -< _4
    _6 <- iPre 1.8327073132961003 -< _5
    _7 <- FRP.Yampa.arr (+1) -< _1
    _4 <- iPre 0.28456547834702023 -< _0
  _8 <- FRP.Yampa.arr (+1) -< _3
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _5)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _6)
  _11 <- iPre 3.7768066229684987 -< _7
  _12 <- FRP.Yampa.arr (+1) -< _9
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _2)
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _11)
  _15 <- iPre 9.035753958019767e-2 -< _14
  _16 <- FRP.Yampa.arr (+1) -< _13
  _17 <- iPre 4.437370869902578 -< _12
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _17)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _16)
  _20 <- iPre 0.9326715770519911 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- pre1 1.3489706223561213 -< _1
  rec
    _5 <- arr11 (+1) -< _4
    _6 <- pre1 1.8327073132961003 -< _5
    _7 <- arr11 (+1) -< _1
    _4 <- pre1 0.28456547834702023 -< _0

  _8 <- arr11 (+1) -< _3
  _9 <- arr21 (-) -< {_0, _5}
  _10 <- arr21 (*) -< {_1, _6}
  _11 <- pre1 3.7768066229684987 -< _7
  _12 <- arr11 (+1) -< _9
  _13 <- arr21 (-) -< {_8, _2}
  _14 <- arr21 (-) -< {_10, _11}
  _15 <- pre1 9.035753958019767e-2 -< _14
  _16 <- arr11 (+1) -< _13
  _17 <- pre1 4.437370869902578 -< _12
  _18 <- arr21 (+) -< {_15, _17}
  _19 <- arr21 (-) -< {_18, _16}
  _20 <- pre1 0.9326715770519911 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4