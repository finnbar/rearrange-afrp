{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  rec
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _1)
    _3 <- iPre 2.2040208398898242 -< _2
    _4 <- iPre 2.6088006920409947 -< _3
  _7 <- FRP.Yampa.arr (+1) -< _5
  _8 <- iPre 2.610462826056753 -< _7
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _3)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _2)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _9)
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _10)
  _13 <- iPre 0.286193671717216 -< _8
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _12)
  _15 <- FRP.Yampa.arr (+1) -< _6
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- iPre 0.9900845102437114 -< _15
  _18 <- iPre 1.882445301408024 -< _16
  _19 <- FRP.Yampa.arr (+1) -< _18
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  rec
    _5 <- arr21 (+) -< {_4, _1}
    _6 <- arr21 (-) -< {_4, _1}
    _3 <- pre1 2.2040208398898242 -< _2
    _4 <- pre1 2.6088006920409947 -< _3

  _7 <- arr11 (+1) -< _5
  _8 <- pre1 2.610462826056753 -< _7
  _9 <- arr21 (-) -< {_3, _3}
  _10 <- arr21 (-) -< {_7, _2}
  _11 <- arr21 (-) -< {_2, _9}
  _12 <- arr21 (-) -< {_11, _10}
  _13 <- pre1 0.286193671717216 -< _8
  _14 <- arr21 (-) -< {_13, _12}
  _15 <- arr11 (+1) -< _6
  _16 <- arr11 (+1) -< _14
  _17 <- pre1 0.9900845102437114 -< _15
  _18 <- pre1 1.882445301408024 -< _16
  _19 <- arr11 (+1) -< _18
  _20 <- arr21 (+) -< {_17, _19}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4