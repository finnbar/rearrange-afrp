{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.465247170972581 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (+1) -< _0
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _6 <- iPre 1.3139118649903954 -< _2
  _7 <- iPre 0.4469741246430673 -< _2
  _8 <- FRP.Yampa.arr (+1) -< _5
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _7)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _8)
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _6)
  _12 <- FRP.Yampa.arr (+1) -< _11
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _4)
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _9)
  _15 <- FRP.Yampa.arr (+1) -< _7
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _14)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _16)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _12)
  _19 <- FRP.Yampa.arr (+1) -< _13
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _18)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.465247170972581 -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- arr11 (+1) -< _0
  _4 <- arr21 (-) -< {_2, _2}
  _5 <- arr21 (+) -< {_1, _0}
  _6 <- pre1 1.3139118649903954 -< _2
  _7 <- pre1 0.4469741246430673 -< _2
  _8 <- arr11 (+1) -< _5
  _9 <- arr21 (+) -< {_1, _7}
  _10 <- arr21 (-) -< {_5, _8}
  _11 <- arr21 (+) -< {_3, _6}
  _12 <- arr11 (+1) -< _11
  _13 <- arr21 (*) -< {_8, _4}
  _14 <- arr21 (-) -< {_9, _9}
  _15 <- arr11 (+1) -< _7
  _16 <- arr21 (+) -< {_15, _14}
  _17 <- arr21 (-) -< {_10, _16}
  _18 <- arr21 (*) -< {_17, _12}
  _19 <- arr11 (+1) -< _13
  _20 <- arr21 (-) -< {_19, _18}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0