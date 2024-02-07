{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.15516971262960944 -< _0
  _2 <- iPre 2.2860504541353746 -< _1
  _3 <- iPre 0.7838892192140078 -< _2
  _4 <- FRP.Yampa.arr (+1) -< _2
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _3)
  _6 <- FRP.Yampa.arr (+1) -< _4
  _7 <- iPre 1.0383526715410232 -< _6
  _8 <- FRP.Yampa.arr (+1) -< _5
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _6)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _5)
  _11 <- iPre 0.3897349491545124 -< _7
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _5)
  _13 <- iPre 1.3471421590773245 -< _6
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _11)
  _15 <- iPre 0.283867023727621 -< _14
  _16 <- FRP.Yampa.arr (+1) -< _10
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _12)
  _18 <- iPre 0.5776503072445789 -< _16
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _9)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _19)
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _20)
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _14)
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _8)
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _21)
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _23)
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _22)
  _27 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _25)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _27)
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _28)
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.15516971262960944 -< _0
  _2 <- pre1 2.2860504541353746 -< _1
  _3 <- pre1 0.7838892192140078 -< _2
  _4 <- arr11 (+1) -< _2
  _5 <- arr21 (-) -< {_0, _3}
  _6 <- arr11 (+1) -< _4
  _7 <- pre1 1.0383526715410232 -< _6
  _8 <- arr11 (+1) -< _5
  _9 <- arr21 (*) -< {_7, _6}
  _10 <- arr21 (*) -< {_5, _5}
  _11 <- pre1 0.3897349491545124 -< _7
  _12 <- arr21 (*) -< {_9, _5}
  _13 <- pre1 1.3471421590773245 -< _6
  _14 <- arr21 (-) -< {_13, _11}
  _15 <- pre1 0.283867023727621 -< _14
  _16 <- arr11 (+1) -< _10
  _17 <- arr21 (*) -< {_0, _12}
  _18 <- pre1 0.5776503072445789 -< _16
  _19 <- arr21 (*) -< {_18, _9}
  _20 <- arr21 (*) -< {_17, _19}
  _21 <- arr21 (*) -< {_20, _20}
  _22 <- arr21 (+) -< {_1, _14}
  _23 <- arr21 (-) -< {_18, _8}
  _24 <- arr21 (-) -< {_12, _21}
  _25 <- arr21 (-) -< {_19, _23}
  _26 <- arr21 (-) -< {_14, _22}
  _27 <- arr21 (-) -< {_26, _25}
  _28 <- arr21 (*) -< {_24, _27}
  _29 <- arr21 (*) -< {_15, _28}
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0