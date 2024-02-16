{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _4 <- FRP.Yampa.arr (+1) -< _2
  _5 <- FRP.Yampa.arr (+1) -< _4
  _6 <- iPre 0.6316892621617805 -< _4
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _6)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _7)
  _9 <- iPre 1.5564569321496966 -< _6
  _10 <- FRP.Yampa.arr (+1) -< _9
  _11 <- FRP.Yampa.arr (+1) -< _3
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _10)
  _13 <- iPre 1.0056055540111843 -< _12
  _14 <- iPre 2.369189218949619 -< _13
  _15 <- FRP.Yampa.arr (+1) -< _8
  _16 <- iPre 2.038775946347848 -< _14
  _17 <- FRP.Yampa.arr (+1) -< _11
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _17)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _18)
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (+) -< {_0, _0}
  _3 <- arr21 (*) -< {_0, _1}
  _4 <- arr11 (+1) -< _2
  _5 <- arr11 (+1) -< _4
  _6 <- pre1 0.6316892621617805 -< _4
  _7 <- arr21 (*) -< {_4, _6}
  _8 <- arr21 (*) -< {_0, _7}
  _9 <- pre1 1.5564569321496966 -< _6
  _10 <- arr11 (+1) -< _9
  _11 <- arr11 (+1) -< _3
  _12 <- arr21 (+) -< {_5, _10}
  _13 <- pre1 1.0056055540111843 -< _12
  _14 <- pre1 2.369189218949619 -< _13
  _15 <- arr11 (+1) -< _8
  _16 <- pre1 2.038775946347848 -< _14
  _17 <- arr11 (+1) -< _11
  _18 <- arr21 (+) -< {_16, _17}
  _19 <- arr21 (*) -< {_15, _18}
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0