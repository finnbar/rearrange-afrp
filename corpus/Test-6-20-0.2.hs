{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  rec
    _5 <- FRP.Yampa.arr (+1) -< _0
    _6 <- FRP.Yampa.arr (+1) -< _4
    _3 <- iPre 1.8169312053707083 -< _3
    _4 <- iPre 0.7163180386372315 -< _5
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- iPre 2.6583052813255033 -< _2
  _9 <- iPre 0.6942445908704862 -< _7
  _10 <- iPre 2.0232959453144956 -< _9
  _11 <- iPre 3.546364317569745 -< _8
  _12 <- FRP.Yampa.arr (+1) -< _1
  _13 <- iPre 1.665809490834852 -< _12
  _14 <- FRP.Yampa.arr (+1) -< _4
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _13)
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _15)
  _17 <- iPre 0.2008234817278472 -< _16
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _11)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (*) -< {_1, _1}
  rec
    _5 <- arr11 (+1) -< _0
    _6 <- arr11 (+1) -< _4
    _3 <- pre1 1.8169312053707083 -< _3
    _4 <- pre1 0.7163180386372315 -< _5

  _7 <- arr11 (+1) -< _6
  _8 <- pre1 2.6583052813255033 -< _2
  _9 <- pre1 0.6942445908704862 -< _7
  _10 <- pre1 2.0232959453144956 -< _9
  _11 <- pre1 3.546364317569745 -< _8
  _12 <- arr11 (+1) -< _1
  _13 <- pre1 1.665809490834852 -< _12
  _14 <- arr11 (+1) -< _4
  _15 <- arr21 (-) -< {_10, _13}
  _16 <- arr21 (-) -< {_0, _15}
  _17 <- pre1 0.2008234817278472 -< _16
  _18 <- arr11 (+1) -< _17
  _19 <- arr21 (*) -< {_18, _11}
  _20 <- arr21 (+) -< {_14, _19}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4