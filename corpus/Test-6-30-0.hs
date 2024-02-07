{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- iPre 1.952323844697144 -< _3
  _5 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _1)
  _7 <- iPre 2.7086547594822528 -< _6
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _6)
  _9 <- iPre 2.4205704903513596 -< _7
  _10 <- iPre 1.645217288720795 -< _3
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _11)
  _13 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _9)
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _0)
  _15 <- FRP.Yampa.arr (+1) -< _0
  _16 <- iPre 1.074651370906262 -< _15
  _17 <- iPre 2.111868946677358 -< _11
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _17)
  _19 <- iPre 1.49306225663331 -< _5
  _20 <- FRP.Yampa.arr (+1) -< _8
  _21 <- iPre 0.24445322528088706 -< _20
  _22 <- FRP.Yampa.arr (+1) -< _19
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _22)
  _24 <- FRP.Yampa.arr (+1) -< _18
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _23)
  _26 <- iPre 1.9024647205496938 -< _21
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _25)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _26)
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 1.4305325049584336 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr11 (+1) -< _0
  _3 <- arr11 (+1) -< _2
  _4 <- pre1 1.952323844697144 -< _3
  _5 <- arr21 (*) -< {_0, _0}
  _6 <- arr21 (*) -< {_4, _1}
  _7 <- pre1 2.7086547594822528 -< _6
  _8 <- arr21 (*) -< {_4, _6}
  _9 <- pre1 2.4205704903513596 -< _7
  _10 <- pre1 1.645217288720795 -< _3
  _11 <- arr21 (-) -< {_1, _1}
  _12 <- arr21 (*) -< {_0, _11}
  _13 <- arr21 (*) -< {_10, _9}
  _14 <- arr21 (+) -< {_12, _0}
  _15 <- arr11 (+1) -< _0
  _16 <- pre1 1.074651370906262 -< _15
  _17 <- pre1 2.111868946677358 -< _11
  _18 <- arr21 (+) -< {_16, _17}
  _19 <- pre1 1.49306225663331 -< _5
  _20 <- arr11 (+1) -< _8
  _21 <- pre1 0.24445322528088706 -< _20
  _22 <- arr11 (+1) -< _19
  _23 <- arr21 (*) -< {_14, _22}
  _24 <- arr11 (+1) -< _18
  _25 <- arr21 (-) -< {_24, _23}
  _26 <- pre1 1.9024647205496938 -< _21
  _27 <- arr21 (*) -< {_13, _25}
  _28 <- arr21 (*) -< {_27, _26}
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 1.4305325049584336 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0