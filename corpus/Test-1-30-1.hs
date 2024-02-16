{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 1.9804646058522801 -< _0
    _3 <- iPre 1.6688293677682595 -< _0
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _5)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _4)
    _8 <- iPre 2.986472189924173 -< _6
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- FRP.Yampa.arr (+1) -< _9
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _6)
    _12 <- iPre 5.115414245206863e-2 -< _7
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _8)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _10)
    _15 <- iPre 0.31042459355576957 -< _9
    _16 <- FRP.Yampa.arr (+1) -< _9
    _17 <- FRP.Yampa.arr (+1) -< _1
    _18 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _15)
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _14)
    _20 <- iPre 0.27464759502138514 -< _11
    _21 <- FRP.Yampa.arr (+1) -< _17
    _22 <- FRP.Yampa.arr (+1) -< _0
    _23 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _13)
    _24 <- FRP.Yampa.arr (+1) -< _20
    _25 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _22)
    _26 <- FRP.Yampa.arr (+1) -< _18
    _27 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _16)
    _28 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _27)
    _29 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _28)
    _30 <- iPre 1.3823235021364142 -< _29
    _1 <- iPre 0.6204351327919179 -< _30
  FRP.Yampa.returnA -< _24

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 1.9804646058522801 -< _0
    _3 <- pre1 1.6688293677682595 -< _0
    _4 <- arr21 (-) -< {_2, _3}
    _5 <- arr21 (-) -< {_1, _3}
    _6 <- arr21 (-) -< {_5, _5}
    _7 <- arr21 (*) -< {_6, _4}
    _8 <- pre1 2.986472189924173 -< _6
    _9 <- arr11 (+1) -< _8
    _10 <- arr11 (+1) -< _9
    _11 <- arr21 (-) -< {_2, _6}
    _12 <- pre1 5.115414245206863e-2 -< _7
    _13 <- arr21 (*) -< {_8, _8}
    _14 <- arr21 (+) -< {_6, _10}
    _15 <- pre1 0.31042459355576957 -< _9
    _16 <- arr11 (+1) -< _9
    _17 <- arr11 (+1) -< _1
    _18 <- arr21 (-) -< {_12, _15}
    _19 <- arr21 (-) -< {_8, _14}
    _20 <- pre1 0.27464759502138514 -< _11
    _21 <- arr11 (+1) -< _17
    _22 <- arr11 (+1) -< _0
    _23 <- arr21 (*) -< {_19, _13}
    _24 <- arr11 (+1) -< _20
    _25 <- arr21 (*) -< {_23, _22}
    _26 <- arr11 (+1) -< _18
    _27 <- arr21 (-) -< {_26, _16}
    _28 <- arr21 (+) -< {_21, _27}
    _29 <- arr21 (-) -< {_25, _28}
    _30 <- pre1 1.3823235021364142 -< _29
    _1 <- pre1 0.6204351327919179 -< _30

  GenProc.GeneralisedArrow.returnA -< _24|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30