{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.1036933423599051 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
  _5 <- FRP.Yampa.arr (+1) -< _4
  _6 <- iPre 1.9993873585983593 -< _5
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- iPre 2.968872306460463 -< _7
  _9 <- iPre 2.2346601419778738 -< _8
  _10 <- iPre 2.284475456366754 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.1036933423599051 -< _0
  _2 <- arr11 (+1) -< _1
  _3 <- arr21 (*) -< {_1, _0}
  _4 <- arr21 (-) -< {_2, _3}
  _5 <- arr11 (+1) -< _4
  _6 <- pre1 1.9993873585983593 -< _5
  _7 <- arr11 (+1) -< _6
  _8 <- pre1 2.968872306460463 -< _7
  _9 <- pre1 2.2346601419778738 -< _8
  _10 <- pre1 2.284475456366754 -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0