{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _1)
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _5)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _1)
    _10 <- iPre 2.44017923454704 -< _5
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _3)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _5)
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _1)
    _14 <- iPre 1.6102506657933835 -< _1
    _15 <- iPre 0.37723649703396744 -< _6
    _16 <- FRP.Yampa.arr (+1) -< _3
    _17 <- iPre 2.163821981206571 -< _15
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _4)
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _3)
    _20 <- iPre 1.0873671033615158 -< _12
    _21 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _13)
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _3)
    _23 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _19)
    _24 <- iPre 0.44091147652777707 -< _21
    _25 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
    _26 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _18)
    _27 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _8)
    _28 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _17)
    _29 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _11)
    _30 <- iPre 0.13384289666584187 -< _27
    _1 <- iPre 0.41923267248851925 -< _29
    _2 <- iPre 0.5579093050956279 -< _24
    _3 <- iPre 1.373590672263861 -< _22
    _4 <- iPre 1.16508764833721 -< _9
    _5 <- iPre 0.6424574410137976 -< _26
    _6 <- iPre 1.036200319176826 -< _28
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _7 <- arr21 (*) -< {_6, _1}
    _8 <- arr21 (*) -< {_5, _5}
    _9 <- arr21 (-) -< {_5, _1}
    _10 <- pre1 2.44017923454704 -< _5
    _11 <- arr21 (+) -< {_1, _3}
    _12 <- arr21 (-) -< {_10, _5}
    _13 <- arr21 (-) -< {_3, _1}
    _14 <- pre1 1.6102506657933835 -< _1
    _15 <- pre1 0.37723649703396744 -< _6
    _16 <- arr11 (+1) -< _3
    _17 <- pre1 2.163821981206571 -< _15
    _18 <- arr21 (+) -< {_6, _4}
    _19 <- arr21 (+) -< {_5, _3}
    _20 <- pre1 1.0873671033615158 -< _12
    _21 <- arr21 (*) -< {_20, _13}
    _22 <- arr21 (+) -< {_6, _3}
    _23 <- arr21 (*) -< {_14, _19}
    _24 <- pre1 0.44091147652777707 -< _21
    _25 <- arr21 (-) -< {_0, _2}
    _26 <- arr21 (*) -< {_7, _18}
    _27 <- arr21 (+) -< {_23, _8}
    _28 <- arr21 (-) -< {_25, _17}
    _29 <- arr21 (+) -< {_16, _11}
    _30 <- pre1 0.13384289666584187 -< _27
    _1 <- pre1 0.41923267248851925 -< _29
    _2 <- pre1 0.5579093050956279 -< _24
    _3 <- pre1 1.373590672263861 -< _22
    _4 <- pre1 1.16508764833721 -< _9
    _5 <- pre1 0.6424574410137976 -< _26
    _6 <- pre1 1.036200319176826 -< _28

  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30