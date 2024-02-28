{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _3)
    _5 <- iPre 2.1595016005390097 -< _4
    _6 <- FRP.Yampa.arr (+1) -< _0
    _7 <- iPre 1.4473852949257693 -< _0
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _6)
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _9)
    _1 <- iPre 2.8853648124755593 -< _10
  FRP.Yampa.returnA -< _7

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (+) -< {_1, _1}
    _3 <- arr21 (-) -< {_1, _1}
    _4 <- arr21 (+) -< {_2, _3}
    _5 <- pre1 2.1595016005390097 -< _4
    _6 <- arr11 (+1) -< _0
    _7 <- pre1 1.4473852949257693 -< _0
    _8 <- arr21 (+) -< {_2, _6}
    _9 <- arr11 (+1) -< _8
    _10 <- arr21 (*) -< {_5, _9}
    _1 <- pre1 2.8853648124755593 -< _10

  AFRP.returnA -< _7|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10