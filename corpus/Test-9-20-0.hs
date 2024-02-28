{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
  _4 <- FRP.Yampa.arr (+1) -< _3
  _5 <- FRP.Yampa.arr (+1) -< _3
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _5)
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _4)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _7)
  _9 <- iPre 2.3473951741223815 -< _8
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _0)
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _5)
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _5)
  _14 <- FRP.Yampa.arr (+1) -< _11
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _0)
  _16 <- iPre 2.3800194666850927 -< _14
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _13)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _17)
  _19 <- iPre 0.796613049523202 -< _18
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (*) -< {_1, _1}
  _3 <- arr21 (+) -< {_2, _0}
  _4 <- arr11 (+1) -< _3
  _5 <- arr11 (+1) -< _3
  _6 <- arr21 (*) -< {_4, _5}
  _7 <- arr21 (+) -< {_5, _4}
  _8 <- arr21 (*) -< {_6, _7}
  _9 <- pre1 2.3473951741223815 -< _8
  _10 <- arr21 (+) -< {_0, _2}
  _11 <- arr21 (+) -< {_8, _0}
  _12 <- arr21 (*) -< {_9, _5}
  _13 <- arr21 (+) -< {_12, _5}
  _14 <- arr11 (+1) -< _11
  _15 <- arr21 (+) -< {_10, _0}
  _16 <- pre1 2.3800194666850927 -< _14
  _17 <- arr21 (+) -< {_15, _13}
  _18 <- arr21 (+) -< {_16, _17}
  _19 <- pre1 0.796613049523202 -< _18
  _20 <- arr11 (+1) -< _19
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0