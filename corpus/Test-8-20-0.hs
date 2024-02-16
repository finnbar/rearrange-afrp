{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _3 <- FRP.Yampa.arr (+1) -< _1
  _4 <- FRP.Yampa.arr (+1) -< _0
  _5 <- iPre 2.2217317638557206 -< _1
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _0)
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _0)
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _4)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _8)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _7)
  _11 <- FRP.Yampa.arr (+1) -< _2
  _12 <- iPre 2.768081372962985 -< _10
  _13 <- iPre 4.7873827546315306e-2 -< _9
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _13)
  _15 <- iPre 1.712996432437211 -< _3
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _11)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _16)
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- FRP.Yampa.arr (+1) -< _18
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _12)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (-) -< {_0, _0}
  _3 <- arr11 (+1) -< _1
  _4 <- arr11 (+1) -< _0
  _5 <- pre1 2.2217317638557206 -< _1
  _6 <- arr21 (*) -< {_4, _0}
  _7 <- arr21 (*) -< {_4, _0}
  _8 <- arr21 (-) -< {_1, _4}
  _9 <- arr21 (+) -< {_6, _8}
  _10 <- arr21 (+) -< {_4, _7}
  _11 <- arr11 (+1) -< _2
  _12 <- pre1 2.768081372962985 -< _10
  _13 <- pre1 4.7873827546315306e-2 -< _9
  _14 <- arr21 (-) -< {_5, _13}
  _15 <- pre1 1.712996432437211 -< _3
  _16 <- arr21 (+) -< {_15, _11}
  _17 <- arr21 (-) -< {_14, _16}
  _18 <- arr11 (+1) -< _17
  _19 <- arr11 (+1) -< _18
  _20 <- arr21 (*) -< {_19, _12}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0