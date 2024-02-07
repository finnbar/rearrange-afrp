{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _5 <- iPre 2.051050657952061 -< _2
  _6 <- iPre 2.083886092625371 -< _5
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _6)
  _9 <- iPre 2.695194692273626 -< _2
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _8)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _5)
  _12 <- iPre 2.325392800483929 -< _7
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _1)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _8)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _9)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _14)
  _17 <- iPre 0.2959475463416218 -< _16
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _11)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _17)
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (*) -< {_1, _1}
  _3 <- arr11 (+1) -< _2
  _4 <- arr21 (-) -< {_0, _0}
  _5 <- pre1 2.051050657952061 -< _2
  _6 <- pre1 2.083886092625371 -< _5
  _7 <- arr21 (*) -< {_1, _2}
  _8 <- arr21 (+) -< {_6, _6}
  _9 <- pre1 2.695194692273626 -< _2
  _10 <- arr21 (+) -< {_4, _8}
  _11 <- arr21 (-) -< {_10, _5}
  _12 <- pre1 2.325392800483929 -< _7
  _13 <- arr21 (*) -< {_3, _1}
  _14 <- arr21 (*) -< {_13, _8}
  _15 <- arr21 (+) -< {_6, _9}
  _16 <- arr21 (*) -< {_15, _14}
  _17 <- pre1 0.2959475463416218 -< _16
  _18 <- arr21 (*) -< {_12, _11}
  _19 <- arr21 (+) -< {_18, _17}
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0