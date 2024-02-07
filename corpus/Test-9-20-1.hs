{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _5)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _5)
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _9)
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _3)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _5)
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _7)
    _14 <- iPre 0.13689783837556482 -< _11
    _15 <- FRP.Yampa.arr (+1) -< _8
    _16 <- FRP.Yampa.arr (+1) -< _0
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _7)
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _10)
    _19 <- iPre 1.3621254274357226 -< _1
    _20 <- FRP.Yampa.arr (+1) -< _15
    _1 <- iPre 2.3716682306972756 -< _14
    _2 <- iPre 2.243619143255234 -< _18
    _3 <- iPre 2.4781127161939547 -< _19
    _4 <- iPre 2.6923103931612706 -< _12
    _5 <- iPre 1.0555951283959168 -< _13
    _6 <- iPre 1.750756868584137 -< _20
  FRP.Yampa.returnA -< _17

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _7 <- arr21 (+) -< {_5, _5}
    _8 <- arr21 (+) -< {_2, _5}
    _9 <- arr11 (+1) -< _6
    _10 <- arr21 (*) -< {_5, _9}
    _11 <- arr21 (+) -< {_8, _3}
    _12 <- arr21 (*) -< {_8, _5}
    _13 <- arr21 (+) -< {_8, _7}
    _14 <- pre1 0.13689783837556482 -< _11
    _15 <- arr11 (+1) -< _8
    _16 <- arr11 (+1) -< _0
    _17 <- arr21 (+) -< {_16, _7}
    _18 <- arr21 (*) -< {_4, _10}
    _19 <- pre1 1.3621254274357226 -< _1
    _20 <- arr11 (+1) -< _15
    _1 <- pre1 2.3716682306972756 -< _14
    _2 <- pre1 2.243619143255234 -< _18
    _3 <- pre1 2.4781127161939547 -< _19
    _4 <- pre1 2.6923103931612706 -< _12
    _5 <- pre1 1.0555951283959168 -< _13
    _6 <- pre1 1.750756868584137 -< _20

  GenProc.GeneralisedArrow.returnA -< _17|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20