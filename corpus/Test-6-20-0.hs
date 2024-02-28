{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _4 <- FRP.Yampa.arr (+1) -< _1
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _2)
  _7 <- FRP.Yampa.arr (+1) -< _2
  _8 <- iPre 0.1573364778477534 -< _0
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _3)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _1)
  _11 <- iPre 2.4164609237267602 -< _0
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _1)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _11)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _8)
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _7)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _6)
  _17 <- iPre 9.175789402173402e-2 -< _16
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _17)
  _19 <- FRP.Yampa.arr (+1) -< _13
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (-) -< {_0, _0}
  _3 <- arr21 (-) -< {_0, _0}
  _4 <- arr11 (+1) -< _1
  _5 <- arr21 (+) -< {_2, _0}
  _6 <- arr21 (*) -< {_5, _2}
  _7 <- arr11 (+1) -< _2
  _8 <- pre1 0.1573364778477534 -< _0
  _9 <- arr21 (*) -< {_0, _3}
  _10 <- arr21 (-) -< {_4, _1}
  _11 <- pre1 2.4164609237267602 -< _0
  _12 <- arr21 (-) -< {_9, _1}
  _13 <- arr21 (*) -< {_5, _11}
  _14 <- arr21 (*) -< {_10, _8}
  _15 <- arr21 (-) -< {_14, _7}
  _16 <- arr21 (+) -< {_15, _6}
  _17 <- pre1 9.175789402173402e-2 -< _16
  _18 <- arr21 (+) -< {_12, _17}
  _19 <- arr11 (+1) -< _13
  _20 <- arr21 (*) -< {_18, _19}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0