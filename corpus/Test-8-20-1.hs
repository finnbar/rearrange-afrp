{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (+1) -< _0
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _1)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _1)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _4)
    _8 <- iPre 1.9659618327173516 -< _4
    _9 <- FRP.Yampa.arr (+1) -< _1
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _4)
    _11 <- FRP.Yampa.arr (+1) -< _5
    _12 <- FRP.Yampa.arr (+1) -< _2
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _12)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _7)
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _12)
    _16 <- iPre 1.1602025104372047 -< _14
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _16)
    _18 <- iPre 1.8973859205903811 -< _17
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _15)
    _20 <- FRP.Yampa.arr (+1) -< _19
    _1 <- iPre 2.686456141839688 -< _13
    _2 <- iPre 0.57137183777851 -< _20
    _3 <- iPre 1.3788016800500766 -< _6
  FRP.Yampa.returnA -< _18

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr11 (+1) -< _0
    _5 <- arr21 (+) -< {_4, _1}
    _6 <- arr21 (-) -< {_5, _1}
    _7 <- arr21 (-) -< {_2, _4}
    _8 <- pre1 1.9659618327173516 -< _4
    _9 <- arr11 (+1) -< _1
    _10 <- arr21 (+) -< {_4, _4}
    _11 <- arr11 (+1) -< _5
    _12 <- arr11 (+1) -< _2
    _13 <- arr21 (*) -< {_3, _12}
    _14 <- arr21 (*) -< {_9, _7}
    _15 <- arr21 (-) -< {_10, _12}
    _16 <- pre1 1.1602025104372047 -< _14
    _17 <- arr21 (*) -< {_8, _16}
    _18 <- pre1 1.8973859205903811 -< _17
    _19 <- arr21 (+) -< {_11, _15}
    _20 <- arr11 (+1) -< _19
    _1 <- pre1 2.686456141839688 -< _13
    _2 <- pre1 0.57137183777851 -< _20
    _3 <- pre1 1.3788016800500766 -< _6

  GenProc.GeneralisedArrow.returnA -< _18|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20