{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _2)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _1)
  rec
    _6 <- iPre 0.3996988252656085 -< _0
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _2)
    _8 <- FRP.Yampa.arr (+1) -< _5
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _7)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _3)
    _11 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _6)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _9)
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _12)
    _14 <- FRP.Yampa.arr (+1) -< _4
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _13)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _9)
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _10)
    _18 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _13)
    _19 <- iPre 0.3136698924713773 -< _17
    _5 <- iPre 0.9593125796407577 -< _7
  _20 <- FRP.Yampa.arr (+1) -< _18
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _3)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _20)
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _0)
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _21)
  _25 <- iPre 0.37680509648640076 -< _23
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _25)
  _27 <- iPre 0.8916782218665629 -< _24
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- FRP.Yampa.arr (+1) -< _26
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_28, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- arr21 (+) -< {_1, _1}
  _3 <- arr21 (*) -< {_2, _2}
  _4 <- arr21 (+) -< {_2, _1}
  rec
    _6 <- pre1 0.3996988252656085 -< _0
    _7 <- arr21 (*) -< {_0, _2}
    _8 <- arr11 (+1) -< _5
    _9 <- arr21 (*) -< {_4, _7}
    _10 <- arr21 (+) -< {_9, _3}
    _11 <- arr21 (-) -< {_8, _6}
    _12 <- arr21 (*) -< {_3, _9}
    _13 <- arr21 (*) -< {_12, _12}
    _14 <- arr11 (+1) -< _4
    _15 <- arr21 (*) -< {_12, _13}
    _16 <- arr21 (*) -< {_15, _9}
    _17 <- arr21 (*) -< {_16, _10}
    _18 <- arr21 (-) -< {_12, _13}
    _19 <- pre1 0.3136698924713773 -< _17
    _5 <- pre1 0.9593125796407577 -< _7

  _20 <- arr11 (+1) -< _18
  _21 <- arr21 (*) -< {_11, _3}
  _22 <- arr21 (-) -< {_14, _20}
  _23 <- arr21 (*) -< {_19, _0}
  _24 <- arr21 (+) -< {_9, _21}
  _25 <- pre1 0.37680509648640076 -< _23
  _26 <- arr21 (-) -< {_22, _25}
  _27 <- pre1 0.8916782218665629 -< _24
  _28 <- arr11 (+1) -< _27
  _29 <- arr11 (+1) -< _26
  _30 <- arr21 (+) -< {_28, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 15