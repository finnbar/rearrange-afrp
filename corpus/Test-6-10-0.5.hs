{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  rec
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _0)
    _6 <- FRP.Yampa.arr (+1) -< _0
    _2 <- iPre 0.7381832975666833 -< _6
    _3 <- iPre 0.3548847414291222 -< _5
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _4)
  _8 <- FRP.Yampa.arr (+1) -< _7
  _9 <- iPre 0.22433192893573048 -< _8
  _10 <- iPre 1.7662712111617518 -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  rec
    _4 <- arr21 (-) -< {_1, _3}
    _5 <- arr21 (-) -< {_3, _0}
    _6 <- arr11 (+1) -< _0
    _2 <- pre1 0.7381832975666833 -< _6
    _3 <- pre1 0.3548847414291222 -< _5

  _7 <- arr21 (*) -< {_2, _4}
  _8 <- arr11 (+1) -< _7
  _9 <- pre1 0.22433192893573048 -< _8
  _10 <- pre1 1.7662712111617518 -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5