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
    _3 <- iPre 1.6275495710575076 -< _0
    _4 <- FRP.Yampa.arr (+1) -< _0
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _6 <- iPre 6.83639687659603e-2 -< _0
    _7 <- FRP.Yampa.arr (+1) -< _5
    _8 <- FRP.Yampa.arr (+1) -< _5
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _0)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _7)
    _11 <- FRP.Yampa.arr (+1) -< _3
    _2 <- iPre 0.25013821992941415 -< _10
  _12 <- iPre 0.542465684926293 -< _6
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _4)
  _14 <- iPre 2.13191522235061 -< _13
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _1)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _15)
  _17 <- FRP.Yampa.arr (+1) -< _14
  _18 <- iPre 2.480390922077846 -< _16
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _18)
  _20 <- iPre 0.4296919174479833 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  rec
    _3 <- pre1 1.6275495710575076 -< _0
    _4 <- arr11 (+1) -< _0
    _5 <- arr21 (*) -< {_0, _0}
    _6 <- pre1 6.83639687659603e-2 -< _0
    _7 <- arr11 (+1) -< _5
    _8 <- arr11 (+1) -< _5
    _9 <- arr21 (*) -< {_8, _0}
    _10 <- arr21 (*) -< {_2, _7}
    _11 <- arr11 (+1) -< _3
    _2 <- pre1 0.25013821992941415 -< _10

  _12 <- pre1 0.542465684926293 -< _6
  _13 <- arr21 (*) -< {_12, _4}
  _14 <- pre1 2.13191522235061 -< _13
  _15 <- arr21 (-) -< {_9, _1}
  _16 <- arr21 (*) -< {_11, _15}
  _17 <- arr11 (+1) -< _14
  _18 <- pre1 2.480390922077846 -< _16
  _19 <- arr21 (-) -< {_17, _18}
  _20 <- pre1 0.4296919174479833 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10