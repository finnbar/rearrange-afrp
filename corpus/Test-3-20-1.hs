{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (+1) -< _3
    _5 <- iPre 0.7512766038802695 -< _4
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _5)
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _7)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _8)
    _10 <- iPre 1.7754420962304451 -< _9
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _10)
    _12 <- iPre 0.8915539366445611 -< _11
    _13 <- FRP.Yampa.arr (+1) -< _12
    _14 <- FRP.Yampa.arr (+1) -< _6
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _15)
    _17 <- FRP.Yampa.arr (+1) -< _16
    _18 <- FRP.Yampa.arr (+1) -< _13
    _19 <- iPre 2.9491782945783704 -< _5
    _20 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _18)
    _1 <- iPre 4.82284422481722e-2 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_0, _0}
    _3 <- arr21 (+) -< {_1, _2}
    _4 <- arr11 (+1) -< _3
    _5 <- pre1 0.7512766038802695 -< _4
    _6 <- arr21 (+) -< {_1, _5}
    _7 <- arr21 (+) -< {_2, _2}
    _8 <- arr21 (-) -< {_3, _7}
    _9 <- arr21 (-) -< {_6, _8}
    _10 <- pre1 1.7754420962304451 -< _9
    _11 <- arr21 (-) -< {_2, _10}
    _12 <- pre1 0.8915539366445611 -< _11
    _13 <- arr11 (+1) -< _12
    _14 <- arr11 (+1) -< _6
    _15 <- arr21 (-) -< {_0, _1}
    _16 <- arr21 (*) -< {_14, _15}
    _17 <- arr11 (+1) -< _16
    _18 <- arr11 (+1) -< _13
    _19 <- pre1 2.9491782945783704 -< _5
    _20 <- arr21 (*) -< {_17, _18}
    _1 <- pre1 4.82284422481722e-2 -< _19

  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20