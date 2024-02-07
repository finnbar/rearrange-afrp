{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- iPre 0.7559632398415792 -< _0
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _0)
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _4)
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _0)
  _7 <- iPre 0.23467059363619167 -< _6
  _8 <- iPre 0.7242316773682272 -< _5
  _9 <- iPre 6.56810428462767e-2 -< _8
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _9)
  FRP.Yampa.returnA -< _10

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- pre1 0.7559632398415792 -< _0
  _3 <- arr21 (-) -< {_0, _2}
  _4 <- arr21 (+) -< {_3, _0}
  _5 <- arr21 (*) -< {_1, _4}
  _6 <- arr21 (*) -< {_4, _0}
  _7 <- pre1 0.23467059363619167 -< _6
  _8 <- pre1 0.7242316773682272 -< _5
  _9 <- pre1 6.56810428462767e-2 -< _8
  _10 <- arr21 (+) -< {_7, _9}
  GenProc.GeneralisedArrow.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 0