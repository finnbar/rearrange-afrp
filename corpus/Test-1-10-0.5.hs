{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- iPre 0.5249623649107965 -< _0
    _4 <- iPre 0.4060090278997678 -< _3
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
    _1 <- iPre 1.4143139352545073 -< _1
    _2 <- iPre 0.1787218906963592 -< _5
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _4)
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _3)
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _4)
  _9 <- FRP.Yampa.arr (+1) -< _8
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- pre1 0.5249623649107965 -< _0
    _4 <- pre1 0.4060090278997678 -< _3
    _5 <- arr21 (+) -< {_2, _0}
    _1 <- pre1 1.4143139352545073 -< _1
    _2 <- pre1 0.1787218906963592 -< _5

  _6 <- arr21 (+) -< {_3, _4}
  _7 <- arr21 (*) -< {_6, _3}
  _8 <- arr21 (+) -< {_7, _4}
  _9 <- arr11 (+1) -< _8
  _10 <- arr21 (+) -< {_4, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5