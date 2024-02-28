{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
  _4 <- FRP.Yampa.arr (+1) -< _1
  rec
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _3)
    _7 <- FRP.Yampa.arr (+1) -< _4
    _8 <- iPre 2.332210709158905 -< _5
    _5 <- iPre 1.7369077703466256 -< _8
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _3)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _4)
  _11 <- iPre 4.6411122486197645 -< _8
  _12 <- iPre 2.1903853305053462 -< _9
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _10)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _7)
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _14)
  _16 <- iPre 2.828166909834785 -< _11
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _16)
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- iPre 1.6682485700293777 -< _18
  _20 <- iPre 0.9729376065028104 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (*) -< {_1, _0}
  _3 <- arr21 (+) -< {_2, _0}
  _4 <- arr11 (+1) -< _1
  rec
    _6 <- arr21 (-) -< {_4, _3}
    _7 <- arr11 (+1) -< _4
    _8 <- pre1 2.332210709158905 -< _5
    _5 <- pre1 1.7369077703466256 -< _8

  _9 <- arr21 (*) -< {_5, _3}
  _10 <- arr21 (-) -< {_9, _4}
  _11 <- pre1 4.6411122486197645 -< _8
  _12 <- pre1 2.1903853305053462 -< _9
  _13 <- arr21 (*) -< {_12, _10}
  _14 <- arr21 (+) -< {_13, _7}
  _15 <- arr21 (*) -< {_6, _14}
  _16 <- pre1 2.828166909834785 -< _11
  _17 <- arr21 (-) -< {_15, _16}
  _18 <- arr11 (+1) -< _17
  _19 <- pre1 1.6682485700293777 -< _18
  _20 <- pre1 0.9729376065028104 -< _19
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4