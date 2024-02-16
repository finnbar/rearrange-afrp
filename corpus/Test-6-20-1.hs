{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 2.9020346080090467 -< _1
    _3 <- iPre 2.9878403089873735e-2 -< _1
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- iPre 0.2973531768421309 -< _2
    _6 <- FRP.Yampa.arr (+1) -< _3
    _7 <- iPre 2.531574394777343 -< _1
    _8 <- FRP.Yampa.arr (+1) -< _2
    _9 <- FRP.Yampa.arr (+1) -< _4
    _10 <- iPre 3.7493876050479207e-2 -< _0
    _11 <- iPre 1.82032926135241 -< _7
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _5)
    _13 <- iPre 1.200739417476043 -< _6
    _14 <- FRP.Yampa.arr (+1) -< _9
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _12)
    _16 <- FRP.Yampa.arr (+1) -< _11
    _17 <- iPre 2.621254362242781 -< _16
    _18 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _15)
    _19 <- iPre 0.8819589191440869 -< _13
    _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _17)
    _1 <- iPre 2.4380333274287462 -< _20
  FRP.Yampa.returnA -< _18

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 2.9020346080090467 -< _1
    _3 <- pre1 2.9878403089873735e-2 -< _1
    _4 <- arr11 (+1) -< _1
    _5 <- pre1 0.2973531768421309 -< _2
    _6 <- arr11 (+1) -< _3
    _7 <- pre1 2.531574394777343 -< _1
    _8 <- arr11 (+1) -< _2
    _9 <- arr11 (+1) -< _4
    _10 <- pre1 3.7493876050479207e-2 -< _0
    _11 <- pre1 1.82032926135241 -< _7
    _12 <- arr21 (+) -< {_10, _5}
    _13 <- pre1 1.200739417476043 -< _6
    _14 <- arr11 (+1) -< _9
    _15 <- arr21 (-) -< {_8, _12}
    _16 <- arr11 (+1) -< _11
    _17 <- pre1 2.621254362242781 -< _16
    _18 <- arr21 (-) -< {_14, _15}
    _19 <- pre1 0.8819589191440869 -< _13
    _20 <- arr21 (*) -< {_19, _17}
    _1 <- pre1 2.4380333274287462 -< _20

  GenProc.GeneralisedArrow.returnA -< _18|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20