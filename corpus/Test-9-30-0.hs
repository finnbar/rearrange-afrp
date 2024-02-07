{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (+1) -< _3
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _0)
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _5)
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _3)
  _9 <- FRP.Yampa.arr (+1) -< _0
  _10 <- FRP.Yampa.arr (+1) -< _3
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _10)
  _12 <- iPre 1.363810009753552 -< _8
  _13 <- iPre 2.6694186951408065 -< _9
  _14 <- FRP.Yampa.arr (+1) -< _12
  _15 <- FRP.Yampa.arr (+1) -< _7
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _6)
  _17 <- iPre 1.7317969690023025 -< _14
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _15)
  _19 <- iPre 0.40343935165317535 -< _17
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _4)
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _16)
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _21)
  _23 <- iPre 1.3113488921636096 -< _18
  _24 <- FRP.Yampa.arr (+1) -< _22
  _25 <- iPre 0.3374501169222074 -< _23
  _26 <- iPre 2.5034523412958776 -< _25
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _24)
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _11)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- FRP.Yampa.arr (+1) -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (-) -< {_1, _0}
  _3 <- arr11 (+1) -< _2
  _4 <- arr11 (+1) -< _3
  _5 <- arr21 (-) -< {_4, _0}
  _6 <- arr21 (+) -< {_2, _0}
  _7 <- arr21 (+) -< {_6, _5}
  _8 <- arr21 (+) -< {_7, _3}
  _9 <- arr11 (+1) -< _0
  _10 <- arr11 (+1) -< _3
  _11 <- arr21 (-) -< {_3, _10}
  _12 <- pre1 1.363810009753552 -< _8
  _13 <- pre1 2.6694186951408065 -< _9
  _14 <- arr11 (+1) -< _12
  _15 <- arr11 (+1) -< _7
  _16 <- arr21 (+) -< {_13, _6}
  _17 <- pre1 1.7317969690023025 -< _14
  _18 <- arr21 (*) -< {_17, _15}
  _19 <- pre1 0.40343935165317535 -< _17
  _20 <- arr21 (*) -< {_6, _4}
  _21 <- arr21 (-) -< {_19, _16}
  _22 <- arr21 (+) -< {_20, _21}
  _23 <- pre1 1.3113488921636096 -< _18
  _24 <- arr11 (+1) -< _22
  _25 <- pre1 0.3374501169222074 -< _23
  _26 <- pre1 2.5034523412958776 -< _25
  _27 <- arr21 (+) -< {_26, _24}
  _28 <- arr21 (+) -< {_27, _11}
  _29 <- arr11 (+1) -< _28
  _30 <- arr11 (+1) -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0