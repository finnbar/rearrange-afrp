{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _5 <- FRP.Yampa.arr (+1) -< _4
    _6 <- FRP.Yampa.arr (+1) -< _4
    _7 <- FRP.Yampa.arr (+1) -< _4
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _2)
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _2)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _1)
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _10)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _10)
    _13 <- iPre 0.5374765379525459 -< _2
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _0)
    _15 <- iPre 0.8452704528359638 -< _8
    _16 <- FRP.Yampa.arr (+1) -< _3
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _11)
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _16)
    _19 <- FRP.Yampa.arr (+1) -< _7
    _20 <- FRP.Yampa.arr (+1) -< _0
    _21 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _13)
    _22 <- FRP.Yampa.arr (+1) -< _17
    _23 <- iPre 2.6415254593373474 -< _20
    _24 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _9)
    _25 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _10)
    _26 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _22)
    _27 <- iPre 0.13682935674134136 -< _26
    _28 <- FRP.Yampa.arr (+1) -< _14
    _29 <- iPre 2.6992105518539944 -< _23
    _30 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _28)
    _1 <- iPre 1.3999578604314726 -< _29
    _2 <- iPre 7.772251457941985e-2 -< _19
    _3 <- iPre 2.702885685261214 -< _25
    _4 <- iPre 2.672604053041269 -< _30
  FRP.Yampa.returnA -< _24

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _5 <- arr11 (+1) -< _4
    _6 <- arr11 (+1) -< _4
    _7 <- arr11 (+1) -< _4
    _8 <- arr21 (+) -< {_6, _2}
    _9 <- arr21 (*) -< {_5, _2}
    _10 <- arr21 (*) -< {_9, _1}
    _11 <- arr21 (+) -< {_5, _10}
    _12 <- arr21 (-) -< {_9, _10}
    _13 <- pre1 0.5374765379525459 -< _2
    _14 <- arr21 (+) -< {_12, _0}
    _15 <- pre1 0.8452704528359638 -< _8
    _16 <- arr11 (+1) -< _3
    _17 <- arr21 (*) -< {_15, _11}
    _18 <- arr21 (*) -< {_13, _16}
    _19 <- arr11 (+1) -< _7
    _20 <- arr11 (+1) -< _0
    _21 <- arr21 (+) -< {_15, _13}
    _22 <- arr11 (+1) -< _17
    _23 <- pre1 2.6415254593373474 -< _20
    _24 <- arr21 (+) -< {_13, _9}
    _25 <- arr21 (*) -< {_18, _10}
    _26 <- arr21 (*) -< {_21, _22}
    _27 <- pre1 0.13682935674134136 -< _26
    _28 <- arr11 (+1) -< _14
    _29 <- pre1 2.6992105518539944 -< _23
    _30 <- arr21 (*) -< {_27, _28}
    _1 <- pre1 1.3999578604314726 -< _29
    _2 <- pre1 7.772251457941985e-2 -< _19
    _3 <- pre1 2.702885685261214 -< _25
    _4 <- pre1 2.672604053041269 -< _30

  GenProc.GeneralisedArrow.returnA -< _24|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30