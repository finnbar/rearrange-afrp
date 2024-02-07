{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.1388103900451787 -< _0
  _2 <- iPre 1.9262550853545914 -< _0
  _3 <- iPre 2.663226048016406 -< _1
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
  rec
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _1)
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _5)
    _5 <- iPre 0.9515072221848062 -< _3
    _6 <- iPre 3.0044474182932155 -< _8
  _9 <- FRP.Yampa.arr (+1) -< _4
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _3)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _4)
  _12 <- FRP.Yampa.arr (+1) -< _10
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _7)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _12)
  _15 <- FRP.Yampa.arr (+1) -< _14
  _16 <- FRP.Yampa.arr (+1) -< _15
  _17 <- iPre 0.7620824844223253 -< _11
  _18 <- iPre 1.0867558185952026 -< _16
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _18)
  _20 <- iPre 2.1790315673914478 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.1388103900451787 -< _0
  _2 <- pre1 1.9262550853545914 -< _0
  _3 <- pre1 2.663226048016406 -< _1
  _4 <- arr21 (-) -< {_2, _3}
  rec
    _7 <- arr21 (-) -< {_3, _1}
    _8 <- arr21 (-) -< {_2, _5}
    _5 <- pre1 0.9515072221848062 -< _3
    _6 <- pre1 3.0044474182932155 -< _8

  _9 <- arr11 (+1) -< _4
  _10 <- arr21 (-) -< {_8, _3}
  _11 <- arr21 (-) -< {_9, _4}
  _12 <- arr11 (+1) -< _10
  _13 <- arr21 (*) -< {_6, _7}
  _14 <- arr21 (*) -< {_13, _12}
  _15 <- arr11 (+1) -< _14
  _16 <- arr11 (+1) -< _15
  _17 <- pre1 0.7620824844223253 -< _11
  _18 <- pre1 1.0867558185952026 -< _16
  _19 <- arr21 (*) -< {_17, _18}
  _20 <- pre1 2.1790315673914478 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4