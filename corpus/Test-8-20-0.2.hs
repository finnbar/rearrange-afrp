{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 0.536113492178474 -< _1
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
    _1 <- iPre 1.3520775581014612 -< _1
  _5 <- FRP.Yampa.arr (+1) -< _4
  _6 <- iPre 0.9763934733719307 -< _3
  _7 <- iPre 1.16971399377793 -< _5
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _0)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _9)
  _11 <- iPre 0.6270344261288378 -< _9
  _12 <- iPre 1.8647127524215803 -< _7
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _8)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _11)
  _15 <- iPre 0.9708630971373818 -< _14
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _15)
  _17 <- iPre 1.7225778973055308 -< _16
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _18)
  _20 <- iPre 0.6645524530168768 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 0.536113492178474 -< _1
    _3 <- arr21 (+) -< {_1, _2}
    _4 <- arr21 (*) -< {_2, _1}
    _1 <- pre1 1.3520775581014612 -< _1

  _5 <- arr11 (+1) -< _4
  _6 <- pre1 0.9763934733719307 -< _3
  _7 <- pre1 1.16971399377793 -< _5
  _8 <- arr21 (*) -< {_7, _0}
  _9 <- arr21 (+) -< {_0, _2}
  _10 <- arr21 (-) -< {_6, _9}
  _11 <- pre1 0.6270344261288378 -< _9
  _12 <- pre1 1.8647127524215803 -< _7
  _13 <- arr21 (*) -< {_0, _8}
  _14 <- arr21 (+) -< {_10, _11}
  _15 <- pre1 0.9708630971373818 -< _14
  _16 <- arr21 (+) -< {_13, _15}
  _17 <- pre1 1.7225778973055308 -< _16
  _18 <- arr11 (+1) -< _17
  _19 <- arr21 (*) -< {_12, _18}
  _20 <- pre1 0.6645524530168768 -< _19
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4