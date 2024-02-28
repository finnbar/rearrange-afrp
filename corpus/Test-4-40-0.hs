{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
  _4 <- iPre 2.6962815186419333 -< _3
  _5 <- iPre 0.7456666018890415 -< _4
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _0)
  _7 <- iPre 2.8184827547169617 -< _6
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _5)
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _8)
  _10 <- iPre 2.6125729999494207 -< _3
  _11 <- FRP.Yampa.arr (+1) -< _10
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _11)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _12)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _3)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _14)
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _9)
  _17 <- FRP.Yampa.arr (+1) -< _5
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _15)
  _19 <- FRP.Yampa.arr (+1) -< _2
  _20 <- FRP.Yampa.arr (+1) -< _19
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _11)
  _22 <- FRP.Yampa.arr (+1) -< _18
  _23 <- FRP.Yampa.arr (+1) -< _16
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _17)
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _14)
  _26 <- iPre 1.4936210102673173 -< _13
  _27 <- FRP.Yampa.arr (+1) -< _3
  _28 <- iPre 1.0564158398241605 -< _24
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _18)
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _13)
  _31 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _29)
  _32 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _25)
  _33 <- FRP.Yampa.arr (uncurry (*)) -< (_31, _23)
  _34 <- iPre 2.154021543005152 -< _28
  _35 <- FRP.Yampa.arr (uncurry (+)) -< (_32, _33)
  _36 <- iPre 1.2763754914780012 -< _34
  _37 <- FRP.Yampa.arr (uncurry (*)) -< (_36, _26)
  _38 <- FRP.Yampa.arr (+1) -< _37
  _39 <- FRP.Yampa.arr (+1) -< _38
  _40 <- FRP.Yampa.arr (uncurry (+)) -< (_35, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (-) -< {_0, _1}
  _3 <- arr21 (*) -< {_2, _1}
  _4 <- pre1 2.6962815186419333 -< _3
  _5 <- pre1 0.7456666018890415 -< _4
  _6 <- arr21 (+) -< {_5, _0}
  _7 <- pre1 2.8184827547169617 -< _6
  _8 <- arr21 (-) -< {_7, _5}
  _9 <- arr21 (-) -< {_6, _8}
  _10 <- pre1 2.6125729999494207 -< _3
  _11 <- arr11 (+1) -< _10
  _12 <- arr21 (*) -< {_9, _11}
  _13 <- arr21 (*) -< {_2, _12}
  _14 <- arr21 (*) -< {_3, _3}
  _15 <- arr21 (+) -< {_13, _14}
  _16 <- arr21 (-) -< {_10, _9}
  _17 <- arr11 (+1) -< _5
  _18 <- arr21 (+) -< {_10, _15}
  _19 <- arr11 (+1) -< _2
  _20 <- arr11 (+1) -< _19
  _21 <- arr21 (-) -< {_10, _11}
  _22 <- arr11 (+1) -< _18
  _23 <- arr11 (+1) -< _16
  _24 <- arr21 (*) -< {_20, _17}
  _25 <- arr21 (*) -< {_21, _14}
  _26 <- pre1 1.4936210102673173 -< _13
  _27 <- arr11 (+1) -< _3
  _28 <- pre1 1.0564158398241605 -< _24
  _29 <- arr21 (-) -< {_7, _18}
  _30 <- arr21 (+) -< {_22, _13}
  _31 <- arr21 (*) -< {_27, _29}
  _32 <- arr21 (+) -< {_30, _25}
  _33 <- arr21 (*) -< {_31, _23}
  _34 <- pre1 2.154021543005152 -< _28
  _35 <- arr21 (+) -< {_32, _33}
  _36 <- pre1 1.2763754914780012 -< _34
  _37 <- arr21 (*) -< {_36, _26}
  _38 <- arr11 (+1) -< _37
  _39 <- arr11 (+1) -< _38
  _40 <- arr21 (+) -< {_35, _39}
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0