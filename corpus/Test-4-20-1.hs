{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (+1) -< _1
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _0)
    _5 <- FRP.Yampa.arr (+1) -< _4
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _4)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _6)
    _8 <- FRP.Yampa.arr (+1) -< _4
    _9 <- FRP.Yampa.arr (+1) -< _7
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _9)
    _11 <- iPre 2.6364887764019027 -< _10
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _0)
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _12)
    _14 <- FRP.Yampa.arr (+1) -< _13
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _9)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _15)
    _17 <- FRP.Yampa.arr (+1) -< _16
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _1)
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _18)
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _19)
    _1 <- iPre 0.9925230759466375 -< _12
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _1
    _3 <- arr21 (+) -< {_1, _2}
    _4 <- arr21 (-) -< {_3, _0}
    _5 <- arr11 (+1) -< _4
    _6 <- arr21 (-) -< {_2, _4}
    _7 <- arr21 (*) -< {_5, _6}
    _8 <- arr11 (+1) -< _4
    _9 <- arr11 (+1) -< _7
    _10 <- arr21 (-) -< {_8, _9}
    _11 <- pre1 2.6364887764019027 -< _10
    _12 <- arr21 (-) -< {_11, _0}
    _13 <- arr21 (+) -< {_12, _12}
    _14 <- arr11 (+1) -< _13
    _15 <- arr21 (*) -< {_14, _9}
    _16 <- arr21 (*) -< {_5, _15}
    _17 <- arr11 (+1) -< _16
    _18 <- arr21 (+) -< {_17, _1}
    _19 <- arr21 (+) -< {_12, _18}
    _20 <- arr21 (-) -< {_11, _19}
    _1 <- pre1 0.9925230759466375 -< _12

  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20