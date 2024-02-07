{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- iPre 0.8134717597896097 -< _0
  rec
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _6)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _0)
    _10 <- iPre 1.2601408858876169 -< _7
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _4)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _5)
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _7)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _2)
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _6)
    _16 <- FRP.Yampa.arr (+1) -< _2
    _17 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _8)
    _18 <- iPre 0.8778196759415969 -< _5
    _4 <- iPre 0.924043206111573 -< _12
    _5 <- iPre 1.023844481062587 -< _12
    _6 <- iPre 1.949452215487648 -< _1
    _7 <- iPre 0.15274292140609785 -< _9
  _19 <- iPre 3.029061791091432 -< _13
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _17)
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _0)
  _22 <- iPre 0.7320112692490152 -< _18
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _16)
  _24 <- iPre 0.5131072617043017 -< _22
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _20)
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _25)
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _23)
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _11)
  _29 <- iPre 1.2347281697968764 -< _28
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr11 (+1) -< _1
  _3 <- pre1 0.8134717597896097 -< _0
  rec
    _8 <- arr21 (*) -< {_5, _6}
    _9 <- arr21 (-) -< {_3, _0}
    _10 <- pre1 1.2601408858876169 -< _7
    _11 <- arr21 (*) -< {_4, _4}
    _12 <- arr21 (*) -< {_9, _5}
    _13 <- arr21 (+) -< {_7, _7}
    _14 <- arr21 (+) -< {_3, _2}
    _15 <- arr21 (-) -< {_12, _6}
    _16 <- arr11 (+1) -< _2
    _17 <- arr21 (-) -< {_10, _8}
    _18 <- pre1 0.8778196759415969 -< _5
    _4 <- pre1 0.924043206111573 -< _12
    _5 <- pre1 1.023844481062587 -< _12
    _6 <- pre1 1.949452215487648 -< _1
    _7 <- pre1 0.15274292140609785 -< _9

  _19 <- pre1 3.029061791091432 -< _13
  _20 <- arr21 (+) -< {_0, _17}
  _21 <- arr21 (*) -< {_14, _0}
  _22 <- pre1 0.7320112692490152 -< _18
  _23 <- arr21 (+) -< {_19, _16}
  _24 <- pre1 0.5131072617043017 -< _22
  _25 <- arr21 (+) -< {_15, _20}
  _26 <- arr21 (-) -< {_24, _25}
  _27 <- arr21 (*) -< {_21, _23}
  _28 <- arr21 (+) -< {_26, _11}
  _29 <- pre1 1.2347281697968764 -< _28
  _30 <- arr21 (+) -< {_27, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15