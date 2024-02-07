{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _2)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _2)
  rec
    _7 <- iPre 1.476418116024954 -< _4
    _8 <- FRP.Yampa.arr (+1) -< _6
    _5 <- iPre 2.2316203333330793 -< _7
    _6 <- iPre 0.7943970509661569 -< _2
  _9 <- FRP.Yampa.arr (+1) -< _2
  _10 <- iPre 2.894878142254033 -< _6
  _11 <- FRP.Yampa.arr (+1) -< _2
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _10)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _8)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _7)
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _5)
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _12)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _14)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _17)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _18)
  _20 <- iPre 3.421667270182133 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (-) -< {_1, _1}
  _3 <- arr21 (*) -< {_0, _2}
  _4 <- arr21 (+) -< {_3, _2}
  rec
    _7 <- pre1 1.476418116024954 -< _4
    _8 <- arr11 (+1) -< _6
    _5 <- pre1 2.2316203333330793 -< _7
    _6 <- pre1 0.7943970509661569 -< _2

  _9 <- arr11 (+1) -< _2
  _10 <- pre1 2.894878142254033 -< _6
  _11 <- arr11 (+1) -< _2
  _12 <- arr21 (*) -< {_6, _10}
  _13 <- arr21 (*) -< {_1, _8}
  _14 <- arr21 (+) -< {_13, _7}
  _15 <- arr21 (-) -< {_4, _5}
  _16 <- arr21 (-) -< {_11, _12}
  _17 <- arr21 (-) -< {_9, _14}
  _18 <- arr21 (+) -< {_15, _17}
  _19 <- arr21 (+) -< {_16, _18}
  _20 <- pre1 3.421667270182133 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4