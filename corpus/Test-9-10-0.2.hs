{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- iPre 1.0421923910201096 -< _1
  rec
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _3)
    _3 <- iPre 4.173401183742893 -< _4
  _5 <- iPre 5.0483988611771276 -< _2
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _4)
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _6)
  _8 <- iPre 3.885795685980588 -< _7
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _8)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _4)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- pre1 1.0421923910201096 -< _1
  rec
    _4 <- arr21 (+) -< {_2, _3}
    _3 <- pre1 4.173401183742893 -< _4

  _5 <- pre1 5.0483988611771276 -< _2
  _6 <- arr21 (+) -< {_5, _4}
  _7 <- arr21 (+) -< {_3, _6}
  _8 <- pre1 3.885795685980588 -< _7
  _9 <- arr21 (-) -< {_3, _8}
  _10 <- arr21 (*) -< {_9, _4}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 2