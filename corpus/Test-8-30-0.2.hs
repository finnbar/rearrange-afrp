{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  rec
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _4)
    _7 <- iPre 2.2777491027756858 -< _2
    _8 <- iPre 1.051886320943119 -< _2
    _3 <- iPre 0.8933281940293332 -< _3
    _4 <- iPre 1.4550953095342212 -< _4
    _5 <- iPre 2.8739919486165 -< _3
  _9 <- FRP.Yampa.arr (+1) -< _2
  _10 <- iPre 2.8850566314122084 -< _3
  _11 <- FRP.Yampa.arr (+1) -< _2
  _12 <- iPre 0.6732597972719656 -< _7
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _6)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _9)
  _15 <- FRP.Yampa.arr (+1) -< _12
  _16 <- FRP.Yampa.arr (+1) -< _10
  _17 <- FRP.Yampa.arr (+1) -< _11
  _18 <- iPre 1.547682068287108 -< _15
  _19 <- iPre 1.3663197382218173 -< _14
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _19)
  _21 <- iPre 2.0161353870661043 -< _17
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _20)
  _23 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _21)
  _24 <- iPre 1.1094303474624323 -< _16
  _25 <- iPre 1.727302390927226 -< _24
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_22, _23)
  _27 <- FRP.Yampa.arr (+1) -< _26
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _28)
  _30 <- iPre 2.6570045214734126 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr11 (+1) -< _0
  rec
    _6 <- arr21 (-) -< {_2, _4}
    _7 <- pre1 2.2777491027756858 -< _2
    _8 <- pre1 1.051886320943119 -< _2
    _3 <- pre1 0.8933281940293332 -< _3
    _4 <- pre1 1.4550953095342212 -< _4
    _5 <- pre1 2.8739919486165 -< _3

  _9 <- arr11 (+1) -< _2
  _10 <- pre1 2.8850566314122084 -< _3
  _11 <- arr11 (+1) -< _2
  _12 <- pre1 0.6732597972719656 -< _7
  _13 <- arr21 (+) -< {_8, _6}
  _14 <- arr21 (*) -< {_13, _9}
  _15 <- arr11 (+1) -< _12
  _16 <- arr11 (+1) -< _10
  _17 <- arr11 (+1) -< _11
  _18 <- pre1 1.547682068287108 -< _15
  _19 <- pre1 1.3663197382218173 -< _14
  _20 <- arr21 (+) -< {_1, _19}
  _21 <- pre1 2.0161353870661043 -< _17
  _22 <- arr21 (*) -< {_5, _20}
  _23 <- arr21 (*) -< {_18, _21}
  _24 <- pre1 1.1094303474624323 -< _16
  _25 <- pre1 1.727302390927226 -< _24
  _26 <- arr21 (+) -< {_22, _23}
  _27 <- arr11 (+1) -< _26
  _28 <- arr11 (+1) -< _27
  _29 <- arr21 (+) -< {_25, _28}
  _30 <- pre1 2.6570045214734126 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6