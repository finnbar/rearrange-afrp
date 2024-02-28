{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.4416466212797094 -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
    _5 <- FRP.Yampa.arr (+1) -< _2
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _0)
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _6)
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _6)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _5)
    _10 <- iPre 0.6302860237898282 -< _5
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _10)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _11)
    _3 <- iPre 0.11830183924833348 -< _12
  _13 <- FRP.Yampa.arr (+1) -< _5
  _14 <- iPre 2.4629468781497685 -< _13
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _13)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _15)
  _17 <- iPre 1.4412160125686853 -< _10
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _17)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _18)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.4416466212797094 -< _0
  _2 <- arr21 (-) -< {_1, _0}
  rec
    _4 <- arr21 (-) -< {_2, _3}
    _5 <- arr11 (+1) -< _2
    _6 <- arr21 (+) -< {_5, _0}
    _7 <- arr21 (+) -< {_4, _6}
    _8 <- arr21 (-) -< {_7, _6}
    _9 <- arr21 (-) -< {_8, _5}
    _10 <- pre1 0.6302860237898282 -< _5
    _11 <- arr21 (-) -< {_9, _10}
    _12 <- arr21 (-) -< {_5, _11}
    _3 <- pre1 0.11830183924833348 -< _12

  _13 <- arr11 (+1) -< _5
  _14 <- pre1 2.4629468781497685 -< _13
  _15 <- arr21 (+) -< {_14, _13}
  _16 <- arr21 (+) -< {_6, _15}
  _17 <- pre1 1.4412160125686853 -< _10
  _18 <- arr21 (-) -< {_16, _17}
  _19 <- arr21 (*) -< {_0, _18}
  _20 <- arr21 (-) -< {_8, _19}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10