{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.1784079694701415 -< _0
  rec
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _1)
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _2)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _3)
    _8 <- iPre 1.7573452967366232 -< _0
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
    _10 <- iPre 2.1719482732456914 -< _9
    _11 <- iPre 1.1821046225277534 -< _5
    _2 <- iPre 0.27882596318630515 -< _2
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _5)
  _13 <- FRP.Yampa.arr (+1) -< _11
  _14 <- iPre 1.2851147637269533 -< _13
  _15 <- FRP.Yampa.arr (+1) -< _8
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _6)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _12)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _7)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _15)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _18)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.1784079694701415 -< _0
  rec
    _3 <- arr21 (+) -< {_2, _1}
    _4 <- arr21 (+) -< {_2, _0}
    _5 <- arr21 (+) -< {_0, _1}
    _6 <- arr21 (*) -< {_2, _2}
    _7 <- arr21 (-) -< {_4, _3}
    _8 <- pre1 1.7573452967366232 -< _0
    _9 <- arr21 (+) -< {_4, _1}
    _10 <- pre1 2.1719482732456914 -< _9
    _11 <- pre1 1.1821046225277534 -< _5
    _2 <- pre1 0.27882596318630515 -< _2

  _12 <- arr21 (-) -< {_3, _5}
  _13 <- arr11 (+1) -< _11
  _14 <- pre1 1.2851147637269533 -< _13
  _15 <- arr11 (+1) -< _8
  _16 <- arr21 (*) -< {_14, _6}
  _17 <- arr21 (+) -< {_10, _12}
  _18 <- arr21 (*) -< {_16, _7}
  _19 <- arr21 (*) -< {_17, _15}
  _20 <- arr21 (-) -< {_19, _18}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10