{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  rec
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
    _3 <- iPre 0.8619934266005779 -< _4
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  _6 <- FRP.Yampa.arr (+1) -< _2
  _7 <- iPre 0.7586087675824149 -< _5
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _7)
  _9 <- iPre 0.2689134379037756 -< _3
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (*) -< {_1, _0}
  rec
    _4 <- arr21 (*) -< {_3, _2}
    _3 <- pre1 0.8619934266005779 -< _4

  _5 <- arr21 (+) -< {_1, _1}
  _6 <- arr11 (+1) -< _2
  _7 <- pre1 0.7586087675824149 -< _5
  _8 <- arr21 (*) -< {_6, _7}
  _9 <- pre1 0.2689134379037756 -< _3
  _10 <- arr21 (-) -< {_8, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2