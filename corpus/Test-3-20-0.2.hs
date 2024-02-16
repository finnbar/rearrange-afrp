{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
  _4 <- iPre 1.7205151875647287 -< _3
  rec
    _6 <- iPre 0.8309964807723851 -< _4
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _5)
    _8 <- FRP.Yampa.arr (+1) -< _7
    _5 <- iPre 3.564333589258959 -< _2
  _9 <- FRP.Yampa.arr (+1) -< _4
  _10 <- iPre 1.6373529471990877 -< _9
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _6)
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _6)
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _12)
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _5)
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _0)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _15)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _16)
  _18 <- iPre 1.856130420252292 -< _13
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _18)
  _20 <- iPre 0.6297665208678151 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (*) -< {_0, _1}
  _3 <- arr21 (-) -< {_2, _2}
  _4 <- pre1 1.7205151875647287 -< _3
  rec
    _6 <- pre1 0.8309964807723851 -< _4
    _7 <- arr21 (-) -< {_2, _5}
    _8 <- arr11 (+1) -< _7
    _5 <- pre1 3.564333589258959 -< _2

  _9 <- arr11 (+1) -< _4
  _10 <- pre1 1.6373529471990877 -< _9
  _11 <- arr21 (*) -< {_8, _6}
  _12 <- arr21 (-) -< {_10, _6}
  _13 <- arr21 (-) -< {_4, _12}
  _14 <- arr21 (-) -< {_4, _5}
  _15 <- arr21 (*) -< {_14, _0}
  _16 <- arr21 (+) -< {_15, _15}
  _17 <- arr21 (-) -< {_11, _16}
  _18 <- pre1 1.856130420252292 -< _13
  _19 <- arr21 (*) -< {_17, _18}
  _20 <- pre1 0.6297665208678151 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4