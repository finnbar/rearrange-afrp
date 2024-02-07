{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _3 <- iPre 0.17238399238913102 -< _2
  rec
    _8 <- iPre 2.273313633206538 -< _2
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- iPre 0.44569635404995506 -< _5
    _11 <- FRP.Yampa.arr (+1) -< _10
    _12 <- FRP.Yampa.arr (+1) -< _2
    _13 <- FRP.Yampa.arr (+1) -< _7
    _4 <- iPre 2.8538637796493638 -< _7
    _5 <- iPre 0.37531187605135224 -< _10
    _6 <- iPre 2.800590152346389 -< _12
    _7 <- iPre 0.17681881006690037 -< _3
  _14 <- iPre 0.24166192641249115 -< _13
  _15 <- FRP.Yampa.arr (+1) -< _6
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _11)
  _17 <- FRP.Yampa.arr (+1) -< _9
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _16)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _15)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _17)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (*) -< {_1, _1}
  _3 <- pre1 0.17238399238913102 -< _2
  rec
    _8 <- pre1 2.273313633206538 -< _2
    _9 <- arr11 (+1) -< _8
    _10 <- pre1 0.44569635404995506 -< _5
    _11 <- arr11 (+1) -< _10
    _12 <- arr11 (+1) -< _2
    _13 <- arr11 (+1) -< _7
    _4 <- pre1 2.8538637796493638 -< _7
    _5 <- pre1 0.37531187605135224 -< _10
    _6 <- pre1 2.800590152346389 -< _12
    _7 <- pre1 0.17681881006690037 -< _3

  _14 <- pre1 0.24166192641249115 -< _13
  _15 <- arr11 (+1) -< _6
  _16 <- arr21 (+) -< {_14, _11}
  _17 <- arr11 (+1) -< _9
  _18 <- arr21 (+) -< {_4, _16}
  _19 <- arr21 (-) -< {_18, _15}
  _20 <- arr21 (-) -< {_19, _17}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10