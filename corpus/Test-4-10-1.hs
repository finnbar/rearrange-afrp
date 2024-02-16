{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
    _4 <- iPre 1.9981895208379992 -< _0
    _5 <- iPre 0.376616384742882 -< _4
    _6 <- iPre 0.11447137102056125 -< _5
    _7 <- iPre 1.8624973296622513 -< _4
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _7)
    _9 <- iPre 0.21842045568279503 -< _4
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _9)
    _1 <- iPre 9.950910343100139e-2 -< _10
  FRP.Yampa.returnA -< _3

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_1, _0}
    _3 <- arr21 (+) -< {_2, _0}
    _4 <- pre1 1.9981895208379992 -< _0
    _5 <- pre1 0.376616384742882 -< _4
    _6 <- pre1 0.11447137102056125 -< _5
    _7 <- pre1 1.8624973296622513 -< _4
    _8 <- arr21 (+) -< {_6, _7}
    _9 <- pre1 0.21842045568279503 -< _4
    _10 <- arr21 (+) -< {_8, _9}
    _1 <- pre1 9.950910343100139e-2 -< _10

  GenProc.GeneralisedArrow.returnA -< _3|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10