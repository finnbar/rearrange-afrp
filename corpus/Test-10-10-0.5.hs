{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
    _5 <- iPre 2.5861621217061628 -< _0
    _1 <- iPre 0.5794153076764698 -< _1
    _2 <- iPre 0.17384479504003625 -< _1
  _6 <- iPre 1.3479746305464184 -< _4
  _7 <- iPre 2.928935692945569 -< _3
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _5)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _7)
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr21 (*) -< {_0, _0}
    _4 <- arr21 (+) -< {_0, _2}
    _5 <- pre1 2.5861621217061628 -< _0
    _1 <- pre1 0.5794153076764698 -< _1
    _2 <- pre1 0.17384479504003625 -< _1

  _6 <- pre1 1.3479746305464184 -< _4
  _7 <- pre1 2.928935692945569 -< _3
  _8 <- arr21 (+) -< {_6, _5}
  _9 <- arr21 (+) -< {_8, _7}
  _10 <- arr11 (+1) -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5