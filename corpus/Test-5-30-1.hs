{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _5)
    _7 <- iPre 0.6220589967587069 -< _5
    _8 <- iPre 0.44111440091737464 -< _6
    _9 <- FRP.Yampa.arr (+1) -< _0
    _10 <- iPre 1.0288831982943776 -< _5
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _0)
    _12 <- FRP.Yampa.arr (+1) -< _1
    _13 <- iPre 0.705687484764155 -< _3
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _13)
    _15 <- FRP.Yampa.arr (+1) -< _0
    _16 <- iPre 2.272786551356772 -< _3
    _17 <- FRP.Yampa.arr (+1) -< _1
    _18 <- iPre 1.0995262833887964 -< _0
    _19 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _8)
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _17)
    _21 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _14)
    _22 <- iPre 0.43287646923962975 -< _11
    _23 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _21)
    _24 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _23)
    _25 <- iPre 9.99519274231535e-2 -< _9
    _26 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _24)
    _27 <- iPre 0.7371605995517019 -< _26
    _28 <- FRP.Yampa.arr (+1) -< _27
    _29 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _4)
    _30 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _29)
    _1 <- iPre 2.1232855343818473 -< _10
    _2 <- iPre 1.2832642284531743 -< _2
    _3 <- iPre 2.82505749611204 -< _20
    _4 <- iPre 2.1898939151031245 -< _30
    _5 <- iPre 2.737024766948478 -< _22
  FRP.Yampa.returnA -< _18

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _6 <- arr21 (+) -< {_3, _5}
    _7 <- pre1 0.6220589967587069 -< _5
    _8 <- pre1 0.44111440091737464 -< _6
    _9 <- arr11 (+1) -< _0
    _10 <- pre1 1.0288831982943776 -< _5
    _11 <- arr21 (+) -< {_3, _0}
    _12 <- arr11 (+1) -< _1
    _13 <- pre1 0.705687484764155 -< _3
    _14 <- arr21 (+) -< {_1, _13}
    _15 <- arr11 (+1) -< _0
    _16 <- pre1 2.272786551356772 -< _3
    _17 <- arr11 (+1) -< _1
    _18 <- pre1 1.0995262833887964 -< _0
    _19 <- arr21 (*) -< {_7, _8}
    _20 <- arr21 (+) -< {_0, _17}
    _21 <- arr21 (+) -< {_16, _14}
    _22 <- pre1 0.43287646923962975 -< _11
    _23 <- arr21 (-) -< {_19, _21}
    _24 <- arr21 (*) -< {_15, _23}
    _25 <- pre1 9.99519274231535e-2 -< _9
    _26 <- arr21 (-) -< {_25, _24}
    _27 <- pre1 0.7371605995517019 -< _26
    _28 <- arr11 (+1) -< _27
    _29 <- arr21 (*) -< {_28, _4}
    _30 <- arr21 (*) -< {_12, _29}
    _1 <- pre1 2.1232855343818473 -< _10
    _2 <- pre1 1.2832642284531743 -< _2
    _3 <- pre1 2.82505749611204 -< _20
    _4 <- pre1 2.1898939151031245 -< _30
    _5 <- pre1 2.737024766948478 -< _22

  GenProc.GeneralisedArrow.returnA -< _18|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30