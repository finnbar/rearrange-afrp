{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 2.3725944572655027 -< _0
  rec
    _4 <- iPre 0.20038770514648307 -< _0
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _3)
    _6 <- FRP.Yampa.arr (+1) -< _0
    _2 <- iPre 1.1576195992012388 -< _4
    _3 <- iPre 2.4424659466235963 -< _5
  _7 <- iPre 0.5568208959638608 -< _1
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _2)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _8)
  _10 <- FRP.Yampa.arr (+1) -< _9
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 2.3725944572655027 -< _0
  rec
    _4 <- pre1 0.20038770514648307 -< _0
    _5 <- arr21 (+) -< {_0, _3}
    _6 <- arr11 (+1) -< _0
    _2 <- pre1 1.1576195992012388 -< _4
    _3 <- pre1 2.4424659466235963 -< _5

  _7 <- pre1 0.5568208959638608 -< _1
  _8 <- arr21 (*) -< {_6, _2}
  _9 <- arr21 (*) -< {_7, _8}
  _10 <- arr11 (+1) -< _9
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5