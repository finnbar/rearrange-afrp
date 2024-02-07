{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _6 <- FRP.Yampa.arr (+1) -< _3
    _7 <- iPre 2.6187275419230827 -< _0
    _8 <- iPre 0.8004078016043621 -< _3
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _4)
    _11 <- iPre 4.700493965998652e-2 -< _7
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _8)
    _13 <- iPre 0.8005036189423181 -< _5
    _14 <- FRP.Yampa.arr (+1) -< _9
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _11)
    _16 <- FRP.Yampa.arr (+1) -< _12
    _17 <- FRP.Yampa.arr (+1) -< _8
    _18 <- iPre 0.24618574235411053 -< _9
    _19 <- iPre 0.2710534554070152 -< _16
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _15)
    _1 <- iPre 1.2821289545579648 -< _20
    _2 <- iPre 1.912752596859203 -< _13
    _3 <- iPre 2.886476560742503 -< _10
    _4 <- iPre 1.3560585776678902 -< _18
    _5 <- iPre 1.102700162038732 -< _14
  FRP.Yampa.returnA -< _19

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _6 <- arr11 (+1) -< _3
    _7 <- pre1 2.6187275419230827 -< _0
    _8 <- pre1 0.8004078016043621 -< _3
    _9 <- arr21 (-) -< {_1, _2}
    _10 <- arr21 (+) -< {_6, _4}
    _11 <- pre1 4.700493965998652e-2 -< _7
    _12 <- arr21 (+) -< {_2, _8}
    _13 <- pre1 0.8005036189423181 -< _5
    _14 <- arr11 (+1) -< _9
    _15 <- arr21 (-) -< {_1, _11}
    _16 <- arr11 (+1) -< _12
    _17 <- arr11 (+1) -< _8
    _18 <- pre1 0.24618574235411053 -< _9
    _19 <- pre1 0.2710534554070152 -< _16
    _20 <- arr21 (+) -< {_17, _15}
    _1 <- pre1 1.2821289545579648 -< _20
    _2 <- pre1 1.912752596859203 -< _13
    _3 <- pre1 2.886476560742503 -< _10
    _4 <- pre1 1.3560585776678902 -< _18
    _5 <- pre1 1.102700162038732 -< _14

  GenProc.GeneralisedArrow.returnA -< _19|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20