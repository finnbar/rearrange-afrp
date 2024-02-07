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
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  _4 <- iPre 1.0142842427558725 -< _0
  _5 <- FRP.Yampa.arr (+1) -< _2
  _6 <- FRP.Yampa.arr (+1) -< _0
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _5)
  _8 <- iPre 1.3136353289433618 -< _3
  _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _1)
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _2)
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _3)
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _3)
  _13 <- iPre 1.804702793644622 -< _5
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _4)
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _14)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _6)
  _17 <- iPre 1.151894420470794 -< _13
  _18 <- FRP.Yampa.arr (+1) -< _5
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _9)
  _20 <- FRP.Yampa.arr (+1) -< _17
  _21 <- FRP.Yampa.arr (+1) -< _16
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _19)
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _12)
  _24 <- FRP.Yampa.arr (+1) -< _23
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _21)
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _10)
  _27 <- iPre 0.38324101462657834 -< _26
  _28 <- iPre 2.737352717435524 -< _27
  _29 <- iPre 1.3849938037493892 -< _28
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (*) -< {_1, _1}
  _3 <- arr21 (-) -< {_1, _0}
  _4 <- pre1 1.0142842427558725 -< _0
  _5 <- arr11 (+1) -< _2
  _6 <- arr11 (+1) -< _0
  _7 <- arr21 (-) -< {_1, _5}
  _8 <- pre1 1.3136353289433618 -< _3
  _9 <- arr21 (-) -< {_8, _1}
  _10 <- arr21 (+) -< {_7, _2}
  _11 <- arr21 (+) -< {_0, _3}
  _12 <- arr21 (+) -< {_1, _3}
  _13 <- pre1 1.804702793644622 -< _5
  _14 <- arr21 (+) -< {_8, _4}
  _15 <- arr21 (*) -< {_1, _14}
  _16 <- arr21 (*) -< {_11, _6}
  _17 <- pre1 1.151894420470794 -< _13
  _18 <- arr11 (+1) -< _5
  _19 <- arr21 (+) -< {_15, _9}
  _20 <- arr11 (+1) -< _17
  _21 <- arr11 (+1) -< _16
  _22 <- arr21 (+) -< {_20, _19}
  _23 <- arr21 (-) -< {_22, _12}
  _24 <- arr11 (+1) -< _23
  _25 <- arr21 (-) -< {_18, _21}
  _26 <- arr21 (+) -< {_24, _10}
  _27 <- pre1 0.38324101462657834 -< _26
  _28 <- pre1 2.737352717435524 -< _27
  _29 <- pre1 1.3849938037493892 -< _28
  _30 <- arr21 (-) -< {_25, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0