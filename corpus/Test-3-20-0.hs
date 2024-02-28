{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- iPre 0.4776169539163975 -< _0
  _3 <- iPre 0.13580598899161447 -< _2
  _4 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _3)
  _5 <- iPre 0.38407884848740287 -< _2
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _0)
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _3)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _3)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _7)
  _12 <- iPre 0.7068698552354776 -< _6
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _12)
  _14 <- FRP.Yampa.arr (+1) -< _11
  _15 <- iPre 1.766158169017385 -< _13
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _14)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _4)
  _18 <- FRP.Yampa.arr (+1) -< _17
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _15)
  _20 <- iPre 1.2142744414568907 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- pre1 0.4776169539163975 -< _0
  _3 <- pre1 0.13580598899161447 -< _2
  _4 <- arr21 (-) -< {_3, _3}
  _5 <- pre1 0.38407884848740287 -< _2
  _6 <- arr21 (-) -< {_3, _0}
  _7 <- arr21 (-) -< {_2, _3}
  _8 <- arr21 (+) -< {_2, _2}
  _9 <- arr21 (*) -< {_1, _3}
  _10 <- arr21 (*) -< {_8, _3}
  _11 <- arr21 (-) -< {_5, _7}
  _12 <- pre1 0.7068698552354776 -< _6
  _13 <- arr21 (-) -< {_10, _12}
  _14 <- arr11 (+1) -< _11
  _15 <- pre1 1.766158169017385 -< _13
  _16 <- arr21 (-) -< {_9, _14}
  _17 <- arr21 (-) -< {_16, _4}
  _18 <- arr11 (+1) -< _17
  _19 <- arr21 (+) -< {_18, _15}
  _20 <- pre1 1.2142744414568907 -< _19
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0