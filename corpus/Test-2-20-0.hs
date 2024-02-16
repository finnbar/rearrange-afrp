{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _0)
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  _5 <- FRP.Yampa.arr (+1) -< _1
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _7 <- iPre 2.9658570680427307 -< _1
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _1)
  _9 <- FRP.Yampa.arr (+1) -< _0
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _4)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _3)
  _12 <- iPre 2.8810579979016078 -< _3
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _8)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _5)
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _11)
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _13)
  _17 <- FRP.Yampa.arr (+1) -< _16
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _2)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _17)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _0}
  _3 <- arr21 (+) -< {_1, _0}
  _4 <- arr21 (-) -< {_1, _0}
  _5 <- arr11 (+1) -< _1
  _6 <- arr21 (*) -< {_1, _1}
  _7 <- pre1 2.9658570680427307 -< _1
  _8 <- arr21 (*) -< {_3, _1}
  _9 <- arr11 (+1) -< _0
  _10 <- arr21 (+) -< {_8, _4}
  _11 <- arr21 (-) -< {_7, _3}
  _12 <- pre1 2.8810579979016078 -< _3
  _13 <- arr21 (*) -< {_10, _8}
  _14 <- arr21 (+) -< {_6, _5}
  _15 <- arr21 (*) -< {_12, _11}
  _16 <- arr21 (-) -< {_15, _13}
  _17 <- arr11 (+1) -< _16
  _18 <- arr21 (+) -< {_9, _2}
  _19 <- arr21 (-) -< {_14, _17}
  _20 <- arr21 (-) -< {_18, _19}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0