{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _2)
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _4)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _4)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _4)
    _8 <- iPre 0.41833251269764016 -< _7
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _2)
    _10 <- FRP.Yampa.arr (+1) -< _0
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _5)
    _12 <- FRP.Yampa.arr (+1) -< _6
    _13 <- iPre 2.5264833485918827 -< _10
    _14 <- iPre 1.756797379276239 -< _9
    _15 <- iPre 1.7760322984592383 -< _13
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _11)
    _17 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _12)
    _18 <- FRP.Yampa.arr (+1) -< _8
    _19 <- FRP.Yampa.arr (+1) -< _18
    _20 <- FRP.Yampa.arr (+1) -< _1
    _1 <- iPre 0.17437362351298416 -< _16
    _2 <- iPre 1.1491424871560871 -< _17
    _3 <- iPre 0.4943189622319082 -< _20
  FRP.Yampa.returnA -< _19

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr21 (+) -< {_3, _2}
    _5 <- arr21 (*) -< {_3, _4}
    _6 <- arr21 (*) -< {_4, _4}
    _7 <- arr21 (-) -< {_3, _4}
    _8 <- pre1 0.41833251269764016 -< _7
    _9 <- arr21 (-) -< {_4, _2}
    _10 <- arr11 (+1) -< _0
    _11 <- arr21 (*) -< {_7, _5}
    _12 <- arr11 (+1) -< _6
    _13 <- pre1 2.5264833485918827 -< _10
    _14 <- pre1 1.756797379276239 -< _9
    _15 <- pre1 1.7760322984592383 -< _13
    _16 <- arr21 (+) -< {_15, _11}
    _17 <- arr21 (-) -< {_14, _12}
    _18 <- arr11 (+1) -< _8
    _19 <- arr11 (+1) -< _18
    _20 <- arr11 (+1) -< _1
    _1 <- pre1 0.17437362351298416 -< _16
    _2 <- pre1 1.1491424871560871 -< _17
    _3 <- pre1 0.4943189622319082 -< _20

  GenProc.GeneralisedArrow.returnA -< _19|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20