{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _0)
    _5 <- iPre 1.1241985469433466 -< _1
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _5)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _5)
    _8 <- iPre 2.686610181712126 -< _7
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _3)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _6)
    _1 <- iPre 7.741345235247574e-2 -< _10
  FRP.Yampa.returnA -< _9

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (-) -< {_1, _0}
    _3 <- arr21 (*) -< {_1, _2}
    _4 <- arr21 (-) -< {_3, _0}
    _5 <- pre1 1.1241985469433466 -< _1
    _6 <- arr21 (*) -< {_5, _5}
    _7 <- arr21 (-) -< {_2, _5}
    _8 <- pre1 2.686610181712126 -< _7
    _9 <- arr21 (+) -< {_8, _3}
    _10 <- arr21 (+) -< {_4, _6}
    _1 <- pre1 7.741345235247574e-2 -< _10

  AFRP.returnA -< _9|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10