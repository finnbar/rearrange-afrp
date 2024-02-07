{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _3)
    _6 <- iPre 2.4387885765940776 -< _5
    _7 <- iPre 1.3438792176686596 -< _3
    _8 <- iPre 1.1421242454160003 -< _0
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- iPre 0.3003074617885182 -< _2
    _1 <- iPre 2.9143924313774354 -< _9
    _2 <- iPre 2.9668250989413223 -< _7
    _3 <- iPre 2.7951419285144854 -< _1
    _4 <- iPre 0.7837953329394389 -< _10
  FRP.Yampa.returnA -< _8

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _5 <- arr21 (*) -< {_4, _3}
    _6 <- pre1 2.4387885765940776 -< _5
    _7 <- pre1 1.3438792176686596 -< _3
    _8 <- pre1 1.1421242454160003 -< _0
    _9 <- arr11 (+1) -< _6
    _10 <- pre1 0.3003074617885182 -< _2
    _1 <- pre1 2.9143924313774354 -< _9
    _2 <- pre1 2.9668250989413223 -< _7
    _3 <- pre1 2.7951419285144854 -< _1
    _4 <- pre1 0.7837953329394389 -< _10

  GenProc.GeneralisedArrow.returnA -< _8|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 10