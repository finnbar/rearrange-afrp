{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 0.708067718432002 -< _1
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (+1) -< _2
    _5 <- iPre 2.653164179918532 -< _0
    _6 <- iPre 1.0573702402318088 -< _1
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _5)
    _8 <- iPre 0.39679965308385956 -< _0
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- iPre 0.3914001230979292 -< _3
    _11 <- iPre 1.0463247362152992 -< _10
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _9)
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _11)
    _14 <- iPre 2.082655338802071 -< _7
    _15 <- FRP.Yampa.arr (+1) -< _13
    _16 <- iPre 0.48859709043657634 -< _12
    _17 <- FRP.Yampa.arr (+1) -< _15
    _18 <- FRP.Yampa.arr (+1) -< _17
    _19 <- FRP.Yampa.arr (+1) -< _16
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _19)
    _1 <- iPre 0.20270528372103055 -< _20
  FRP.Yampa.returnA -< _14

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 0.708067718432002 -< _1
    _3 <- arr21 (*) -< {_1, _2}
    _4 <- arr11 (+1) -< _2
    _5 <- pre1 2.653164179918532 -< _0
    _6 <- pre1 1.0573702402318088 -< _1
    _7 <- arr21 (*) -< {_6, _5}
    _8 <- pre1 0.39679965308385956 -< _0
    _9 <- arr11 (+1) -< _6
    _10 <- pre1 0.3914001230979292 -< _3
    _11 <- pre1 1.0463247362152992 -< _10
    _12 <- arr21 (+) -< {_4, _9}
    _13 <- arr21 (*) -< {_8, _11}
    _14 <- pre1 2.082655338802071 -< _7
    _15 <- arr11 (+1) -< _13
    _16 <- pre1 0.48859709043657634 -< _12
    _17 <- arr11 (+1) -< _15
    _18 <- arr11 (+1) -< _17
    _19 <- arr11 (+1) -< _16
    _20 <- arr21 (+) -< {_18, _19}
    _1 <- pre1 0.20270528372103055 -< _20

  AFRP.returnA -< _14|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20