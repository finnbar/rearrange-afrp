{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.168887514644499 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _0
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
  _4 <- FRP.Yampa.arr (+1) -< _0
  _5 <- iPre 3.048646717795857 -< _2
  _6 <- FRP.Yampa.arr (+1) -< _4
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _3)
  _8 <- iPre 3.0131384110758455 -< _6
  _9 <- iPre 1.273032679322846 -< _0
  rec
    _11 <- FRP.Yampa.arr (+1) -< _6
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _3)
    _13 <- FRP.Yampa.arr (+1) -< _10
    _14 <- iPre 2.286619266897346 -< _3
    _15 <- FRP.Yampa.arr (+1) -< _1
    _16 <- FRP.Yampa.arr (+1) -< _13
    _17 <- FRP.Yampa.arr (+1) -< _6
    _10 <- iPre 0.8771393160358172 -< _16
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _1)
  _19 <- iPre 1.7101675116918655 -< _4
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _12)
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _11)
  _22 <- FRP.Yampa.arr (+1) -< _14
  _23 <- FRP.Yampa.arr (+1) -< _20
  _24 <- iPre 0.8884638282729291 -< _21
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _22)
  _26 <- FRP.Yampa.arr (+1) -< _24
  _27 <- iPre 2.783394891745549 -< _9
  _28 <- iPre 1.4450598110293331 -< _19
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _15)
  _30 <- FRP.Yampa.arr (+1) -< _28
  _31 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _8)
  _32 <- FRP.Yampa.arr (+1) -< _29
  _33 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _31)
  _34 <- FRP.Yampa.arr (uncurry (*)) -< (_33, _27)
  _35 <- FRP.Yampa.arr (+1) -< _32
  _36 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _34)
  _37 <- FRP.Yampa.arr (+1) -< _35
  _38 <- FRP.Yampa.arr (+1) -< _37
  _39 <- FRP.Yampa.arr (uncurry (+)) -< (_36, _38)
  _40 <- FRP.Yampa.arr (uncurry (*)) -< (_30, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.168887514644499 -< _0
  _2 <- arr11 (+1) -< _0
  _3 <- arr21 (+) -< {_2, _2}
  _4 <- arr11 (+1) -< _0
  _5 <- pre1 3.048646717795857 -< _2
  _6 <- arr11 (+1) -< _4
  _7 <- arr21 (+) -< {_1, _3}
  _8 <- pre1 3.0131384110758455 -< _6
  _9 <- pre1 1.273032679322846 -< _0
  rec
    _11 <- arr11 (+1) -< _6
    _12 <- arr21 (-) -< {_10, _3}
    _13 <- arr11 (+1) -< _10
    _14 <- pre1 2.286619266897346 -< _3
    _15 <- arr11 (+1) -< _1
    _16 <- arr11 (+1) -< _13
    _17 <- arr11 (+1) -< _6
    _10 <- pre1 0.8771393160358172 -< _16

  _18 <- arr21 (*) -< {_13, _1}
  _19 <- pre1 1.7101675116918655 -< _4
  _20 <- arr21 (+) -< {_17, _12}
  _21 <- arr21 (+) -< {_5, _11}
  _22 <- arr11 (+1) -< _14
  _23 <- arr11 (+1) -< _20
  _24 <- pre1 0.8884638282729291 -< _21
  _25 <- arr21 (*) -< {_23, _22}
  _26 <- arr11 (+1) -< _24
  _27 <- pre1 2.783394891745549 -< _9
  _28 <- pre1 1.4450598110293331 -< _19
  _29 <- arr21 (-) -< {_7, _15}
  _30 <- arr11 (+1) -< _28
  _31 <- arr21 (+) -< {_18, _8}
  _32 <- arr11 (+1) -< _29
  _33 <- arr21 (+) -< {_26, _31}
  _34 <- arr21 (*) -< {_33, _27}
  _35 <- arr11 (+1) -< _32
  _36 <- arr21 (+) -< {_25, _34}
  _37 <- arr11 (+1) -< _35
  _38 <- arr11 (+1) -< _37
  _39 <- arr21 (+) -< {_36, _38}
  _40 <- arr21 (*) -< {_30, _39}
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8