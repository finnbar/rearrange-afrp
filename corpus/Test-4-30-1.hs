{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _9 <- FRP.Yampa.arr (+1) -< _0
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _11 <- FRP.Yampa.arr (+1) -< _1
    _12 <- iPre 1.214972908754628 -< _6
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _11)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _1)
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _10)
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _4)
    _17 <- iPre 1.9992604588470595 -< _1
    _18 <- FRP.Yampa.arr (+1) -< _0
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _16)
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _8)
    _21 <- iPre 2.3130558292334547 -< _16
    _22 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _12)
    _23 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _0)
    _24 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
    _25 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _26 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _22)
    _27 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _21)
    _28 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _9)
    _29 <- iPre 2.291868687353184 -< _24
    _30 <- FRP.Yampa.arr (+1) -< _19
    _1 <- iPre 1.6213006897510227 -< _29
    _2 <- iPre 1.843314182111316 -< _25
    _3 <- iPre 0.5233892197388064 -< _30
    _4 <- iPre 1.1348913007402874 -< _28
    _5 <- iPre 2.87607513243472 -< _17
    _6 <- iPre 1.7857968805840732 -< _27
    _7 <- iPre 1.908776784015331 -< _5
    _8 <- iPre 2.1336701321883003 -< _23
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _9 <- arr11 (+1) -< _0
    _10 <- arr21 (+) -< {_0, _1}
    _11 <- arr11 (+1) -< _1
    _12 <- pre1 1.214972908754628 -< _6
    _13 <- arr21 (+) -< {_11, _11}
    _14 <- arr21 (*) -< {_4, _1}
    _15 <- arr21 (-) -< {_6, _10}
    _16 <- arr21 (+) -< {_14, _4}
    _17 <- pre1 1.9992604588470595 -< _1
    _18 <- arr11 (+1) -< _0
    _19 <- arr21 (-) -< {_7, _16}
    _20 <- arr21 (+) -< {_11, _8}
    _21 <- pre1 2.3130558292334547 -< _16
    _22 <- arr21 (-) -< {_2, _12}
    _23 <- arr21 (+) -< {_18, _0}
    _24 <- arr21 (*) -< {_3, _2}
    _25 <- arr21 (*) -< {_1, _1}
    _26 <- arr21 (+) -< {_13, _22}
    _27 <- arr21 (-) -< {_26, _21}
    _28 <- arr21 (-) -< {_15, _9}
    _29 <- pre1 2.291868687353184 -< _24
    _30 <- arr11 (+1) -< _19
    _1 <- pre1 1.6213006897510227 -< _29
    _2 <- pre1 1.843314182111316 -< _25
    _3 <- pre1 0.5233892197388064 -< _30
    _4 <- pre1 1.1348913007402874 -< _28
    _5 <- pre1 2.87607513243472 -< _17
    _6 <- pre1 1.7857968805840732 -< _27
    _7 <- pre1 1.908776784015331 -< _5
    _8 <- pre1 2.1336701321883003 -< _23

  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30