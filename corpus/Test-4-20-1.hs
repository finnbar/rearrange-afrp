{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _7 <- iPre 0.6609360422119691 -< _1
    _8 <- iPre 2.112132866976867 -< _1
    _9 <- FRP.Yampa.arr (+1) -< _1
    _10 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _10)
    _12 <- FRP.Yampa.arr (+1) -< _2
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _12)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _12)
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _8)
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _6)
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _4)
    _18 <- FRP.Yampa.arr (+1) -< _17
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _16)
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _15)
    _1 <- iPre 2.233632123290716 -< _0
    _2 <- iPre 2.6040594034687223 -< _13
    _3 <- iPre 1.0451691592846226 -< _20
    _4 <- iPre 2.4601343344009963 -< _5
    _5 <- iPre 1.896301805528167 -< _18
    _6 <- iPre 2.879530592557482 -< _14
  FRP.Yampa.returnA -< _19

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _7 <- pre1 0.6609360422119691 -< _1
    _8 <- pre1 2.112132866976867 -< _1
    _9 <- arr11 (+1) -< _1
    _10 <- arr21 (-) -< {_1, _1}
    _11 <- arr21 (*) -< {_9, _10}
    _12 <- arr11 (+1) -< _2
    _13 <- arr21 (*) -< {_8, _12}
    _14 <- arr21 (*) -< {_4, _12}
    _15 <- arr21 (-) -< {_11, _8}
    _16 <- arr21 (+) -< {_11, _6}
    _17 <- arr21 (*) -< {_10, _4}
    _18 <- arr11 (+1) -< _17
    _19 <- arr21 (-) -< {_3, _16}
    _20 <- arr21 (-) -< {_7, _15}
    _1 <- pre1 2.233632123290716 -< _0
    _2 <- pre1 2.6040594034687223 -< _13
    _3 <- pre1 1.0451691592846226 -< _20
    _4 <- pre1 2.4601343344009963 -< _5
    _5 <- pre1 1.896301805528167 -< _18
    _6 <- pre1 2.879530592557482 -< _14

  GenProc.GeneralisedArrow.returnA -< _19|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20