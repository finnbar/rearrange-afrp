{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- iPre 2.1259057906186207 -< _0
  _3 <- iPre 1.267686660310178 -< _2
  rec
    _6 <- FRP.Yampa.arr (+1) -< _0
    _7 <- FRP.Yampa.arr (+1) -< _2
    _8 <- FRP.Yampa.arr (+1) -< _6
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _6)
    _4 <- iPre 2.0060677449320417 -< _4
    _5 <- iPre 0.8214368716583487 -< _5
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _9)
  _11 <- iPre 3.743241674565878 -< _1
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _7)
  _13 <- iPre 1.4653629149933929 -< _10
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _7)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _9)
  _16 <- FRP.Yampa.arr (+1) -< _12
  _17 <- FRP.Yampa.arr (+1) -< _13
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _17)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _14)
  _20 <- FRP.Yampa.arr (+1) -< _16
  _21 <- iPre 3.6834038562343743 -< _19
  _22 <- iPre 3.52843307767454 -< _20
  _23 <- FRP.Yampa.arr (+1) -< _18
  _24 <- iPre 0.6909201518175418 -< _21
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _15)
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _25)
  _27 <- FRP.Yampa.arr (+1) -< _26
  _28 <- iPre 3.251799214646697 -< _22
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_27, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- pre1 2.1259057906186207 -< _0
  _3 <- pre1 1.267686660310178 -< _2
  rec
    _6 <- arr11 (+1) -< _0
    _7 <- arr11 (+1) -< _2
    _8 <- arr11 (+1) -< _6
    _9 <- arr21 (+) -< {_4, _6}
    _4 <- pre1 2.0060677449320417 -< _4
    _5 <- pre1 0.8214368716583487 -< _5

  _10 <- arr21 (*) -< {_3, _9}
  _11 <- pre1 3.743241674565878 -< _1
  _12 <- arr21 (-) -< {_8, _7}
  _13 <- pre1 1.4653629149933929 -< _10
  _14 <- arr21 (+) -< {_9, _7}
  _15 <- arr21 (+) -< {_11, _9}
  _16 <- arr11 (+1) -< _12
  _17 <- arr11 (+1) -< _13
  _18 <- arr21 (-) -< {_4, _17}
  _19 <- arr21 (*) -< {_8, _14}
  _20 <- arr11 (+1) -< _16
  _21 <- pre1 3.6834038562343743 -< _19
  _22 <- pre1 3.52843307767454 -< _20
  _23 <- arr11 (+1) -< _18
  _24 <- pre1 0.6909201518175418 -< _21
  _25 <- arr21 (+) -< {_23, _15}
  _26 <- arr21 (+) -< {_24, _25}
  _27 <- arr11 (+1) -< _26
  _28 <- pre1 3.251799214646697 -< _22
  _29 <- arr11 (+1) -< _28
  _30 <- arr21 (-) -< {_27, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6