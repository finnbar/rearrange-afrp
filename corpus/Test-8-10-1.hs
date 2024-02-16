{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 3.032538093534844 -< _0
    _3 <- iPre 0.835226230549083 -< _1
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
    _5 <- FRP.Yampa.arr (+1) -< _3
    _6 <- iPre 5.675423288357729e-2 -< _3
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _5)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _7)
    _9 <- iPre 2.730529680822617 -< _4
    _10 <- iPre 0.8803968871161129 -< _9
    _1 <- iPre 3.0443312475911446 -< _10
  FRP.Yampa.returnA -< _8

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 3.032538093534844 -< _0
    _3 <- pre1 0.835226230549083 -< _1
    _4 <- arr21 (-) -< {_2, _3}
    _5 <- arr11 (+1) -< _3
    _6 <- pre1 5.675423288357729e-2 -< _3
    _7 <- arr21 (-) -< {_1, _5}
    _8 <- arr21 (*) -< {_6, _7}
    _9 <- pre1 2.730529680822617 -< _4
    _10 <- pre1 0.8803968871161129 -< _9
    _1 <- pre1 3.0443312475911446 -< _10

  GenProc.GeneralisedArrow.returnA -< _8|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10