{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- iPre 2.714489048647421 -< _0
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
  _4 <- FRP.Yampa.arr (+1) -< _0
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _6 <- iPre 1.6102340209481623 -< _2
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _3)
  _8 <- iPre 0.4255227946955007 -< _3
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _3)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _1)
  _11 <- FRP.Yampa.arr (+1) -< _4
  _12 <- iPre 0.5603439253004296 -< _11
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _6)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _13)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _14)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _10)
  _17 <- FRP.Yampa.arr (+1) -< _16
  _18 <- iPre 1.4957276616070772 -< _8
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _18)
  _20 <- iPre 1.513530155126566 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- pre1 2.714489048647421 -< _0
  _3 <- arr21 (*) -< {_1, _2}
  _4 <- arr11 (+1) -< _0
  _5 <- arr21 (*) -< {_0, _0}
  _6 <- pre1 1.6102340209481623 -< _2
  _7 <- arr21 (*) -< {_0, _3}
  _8 <- pre1 0.4255227946955007 -< _3
  _9 <- arr21 (*) -< {_6, _3}
  _10 <- arr21 (+) -< {_5, _1}
  _11 <- arr11 (+1) -< _4
  _12 <- pre1 0.5603439253004296 -< _11
  _13 <- arr21 (+) -< {_12, _6}
  _14 <- arr21 (*) -< {_9, _13}
  _15 <- arr21 (+) -< {_7, _14}
  _16 <- arr21 (*) -< {_15, _10}
  _17 <- arr11 (+1) -< _16
  _18 <- pre1 1.4957276616070772 -< _8
  _19 <- arr21 (-) -< {_17, _18}
  _20 <- pre1 1.513530155126566 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0