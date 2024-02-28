{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
    _3 <- iPre 2.4623949733433412 -< _0
    _4 <- FRP.Yampa.arr (+1) -< _2
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _3)
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _5)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _5)
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _8)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _5)
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _9)
    _12 <- iPre 2.905607376306789 -< _11
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _5)
    _14 <- FRP.Yampa.arr (+1) -< _13
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _14)
    _16 <- iPre 2.4143865064772485 -< _15
    _17 <- FRP.Yampa.arr (+1) -< _5
    _18 <- iPre 1.1608388552413489 -< _8
    _19 <- iPre 0.9766411705132222 -< _3
    _20 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _12)
    _21 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _16)
    _22 <- FRP.Yampa.arr (+1) -< _17
    _23 <- FRP.Yampa.arr (uncurry (-)) -< (_21, _22)
    _24 <- iPre 2.1750162193807543 -< _9
    _25 <- iPre 2.4510622159690705 -< _14
    _26 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _20)
    _27 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _24)
    _28 <- iPre 0.5212997973515175 -< _19
    _29 <- FRP.Yampa.arr (+1) -< _25
    _30 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _29)
    _1 <- iPre 0.8085512311100481 -< _27
  FRP.Yampa.returnA -< _30

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_1, _0}
    _3 <- pre1 2.4623949733433412 -< _0
    _4 <- arr11 (+1) -< _2
    _5 <- arr21 (-) -< {_4, _3}
    _6 <- arr21 (*) -< {_2, _5}
    _7 <- arr21 (*) -< {_6, _5}
    _8 <- arr11 (+1) -< _7
    _9 <- arr21 (-) -< {_4, _8}
    _10 <- arr21 (+) -< {_9, _5}
    _11 <- arr21 (+) -< {_10, _9}
    _12 <- pre1 2.905607376306789 -< _11
    _13 <- arr21 (*) -< {_12, _5}
    _14 <- arr11 (+1) -< _13
    _15 <- arr21 (+) -< {_14, _14}
    _16 <- pre1 2.4143865064772485 -< _15
    _17 <- arr11 (+1) -< _5
    _18 <- pre1 1.1608388552413489 -< _8
    _19 <- pre1 0.9766411705132222 -< _3
    _20 <- arr21 (*) -< {_0, _12}
    _21 <- arr21 (-) -< {_18, _16}
    _22 <- arr11 (+1) -< _17
    _23 <- arr21 (-) -< {_21, _22}
    _24 <- pre1 2.1750162193807543 -< _9
    _25 <- pre1 2.4510622159690705 -< _14
    _26 <- arr21 (-) -< {_23, _20}
    _27 <- arr21 (*) -< {_26, _24}
    _28 <- pre1 0.5212997973515175 -< _19
    _29 <- arr11 (+1) -< _25
    _30 <- arr21 (*) -< {_28, _29}
    _1 <- pre1 0.8085512311100481 -< _27

  AFRP.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30