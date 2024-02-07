{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- iPre 2.6831931381968506 -< _1
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _3)
    _6 <- FRP.Yampa.arr (+1) -< _5
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
    _8 <- iPre 1.9693178399728377 -< _2
    _9 <- FRP.Yampa.arr (+1) -< _4
    _10 <- FRP.Yampa.arr (+1) -< _7
    _1 <- iPre 2.0515782466225105 -< _9
    _2 <- iPre 1.3372118718966353 -< _8
    _3 <- iPre 0.5035711576790273 -< _10
  FRP.Yampa.returnA -< _6

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- pre1 2.6831931381968506 -< _1
    _5 <- arr21 (-) -< {_4, _3}
    _6 <- arr11 (+1) -< _5
    _7 <- arr21 (+) -< {_2, _0}
    _8 <- pre1 1.9693178399728377 -< _2
    _9 <- arr11 (+1) -< _4
    _10 <- arr11 (+1) -< _7
    _1 <- pre1 2.0515782466225105 -< _9
    _2 <- pre1 1.3372118718966353 -< _8
    _3 <- pre1 0.5035711576790273 -< _10

  GenProc.GeneralisedArrow.returnA -< _6|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10