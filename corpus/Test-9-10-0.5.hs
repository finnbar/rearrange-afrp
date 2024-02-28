{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _3 <- iPre 1.5180299333023324 -< _2
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _3)
    _1 <- iPre 1.3179412951752083 -< _4
  _6 <- iPre 0.3548263837905364 -< _3
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _6)
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _5)
  _9 <- iPre 0.7778560037895231 -< _7
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _8)
  FRP.Yampa.returnA -< _10

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (+) -< {_0, _1}
    _3 <- pre1 1.5180299333023324 -< _2
    _4 <- arr21 (-) -< {_2, _2}
    _5 <- arr21 (+) -< {_0, _3}
    _1 <- pre1 1.3179412951752083 -< _4

  _6 <- pre1 0.3548263837905364 -< _3
  _7 <- arr21 (*) -< {_6, _6}
  _8 <- arr21 (-) -< {_3, _5}
  _9 <- pre1 0.7778560037895231 -< _7
  _10 <- arr21 (-) -< {_9, _8}
  AFRP.returnA -< _10|]

codeLen :: Int
codeLen = 10
codeRecLen :: Int
codeRecLen = 5