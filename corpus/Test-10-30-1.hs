{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _4)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _4)
    _7 <- FRP.Yampa.arr (+1) -< _4
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _3)
    _9 <- iPre 2.2643588943545043 -< _7
    _10 <- iPre 0.6755688855516353 -< _2
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _3)
    _12 <- iPre 1.7839693321949994 -< _6
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _0)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _13)
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _14)
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _7)
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _8)
    _18 <- iPre 0.22499339488076975 -< _4
    _19 <- iPre 1.5576872729870699 -< _8
    _20 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _17)
    _21 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _8)
    _22 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _18)
    _23 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _5)
    _24 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _6)
    _25 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _2)
    _26 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _12)
    _27 <- iPre 1.5904352544336382 -< _22
    _28 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _25)
    _29 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _24)
    _30 <- FRP.Yampa.arr (+1) -< _27
    _1 <- iPre 2.1596219320981622 -< _30
    _2 <- iPre 2.502049888639982 -< _28
    _3 <- iPre 2.1032570761795237 -< _26
    _4 <- iPre 2.3857254544338478 -< _29
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _5 <- arr21 (-) -< {_4, _4}
    _6 <- arr21 (*) -< {_4, _4}
    _7 <- arr11 (+1) -< _4
    _8 <- arr21 (-) -< {_0, _3}
    _9 <- pre1 2.2643588943545043 -< _7
    _10 <- pre1 0.6755688855516353 -< _2
    _11 <- arr21 (*) -< {_10, _3}
    _12 <- pre1 1.7839693321949994 -< _6
    _13 <- arr21 (-) -< {_6, _0}
    _14 <- arr21 (*) -< {_11, _13}
    _15 <- arr21 (*) -< {_5, _14}
    _16 <- arr21 (-) -< {_9, _7}
    _17 <- arr21 (*) -< {_7, _8}
    _18 <- pre1 0.22499339488076975 -< _4
    _19 <- pre1 1.5576872729870699 -< _8
    _20 <- arr21 (*) -< {_15, _17}
    _21 <- arr21 (*) -< {_7, _8}
    _22 <- arr21 (*) -< {_18, _18}
    _23 <- arr21 (-) -< {_19, _5}
    _24 <- arr21 (-) -< {_1, _6}
    _25 <- arr21 (-) -< {_16, _2}
    _26 <- arr21 (+) -< {_16, _12}
    _27 <- pre1 1.5904352544336382 -< _22
    _28 <- arr21 (+) -< {_21, _25}
    _29 <- arr21 (-) -< {_23, _24}
    _30 <- arr11 (+1) -< _27
    _1 <- pre1 2.1596219320981622 -< _30
    _2 <- pre1 2.502049888639982 -< _28
    _3 <- pre1 2.1032570761795237 -< _26
    _4 <- pre1 2.3857254544338478 -< _29

  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30