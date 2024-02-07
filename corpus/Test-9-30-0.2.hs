{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _1)
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
    _1 <- iPre 2.3610503747794276 -< _4
    _2 <- iPre 2.9282547582697362 -< _4
  _7 <- FRP.Yampa.arr (+1) -< _4
  _8 <- FRP.Yampa.arr (+1) -< _1
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _8)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _6)
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _7)
  _12 <- iPre 2.4600520047158274 -< _0
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _3)
  _14 <- iPre 6.508747060652083e-2 -< _7
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _3)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _8)
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _12)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _11)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _16)
  _20 <- iPre 7.484119078322501e-2 -< _19
  _21 <- iPre 2.734675890322731 -< _14
  _22 <- iPre 1.0392534188223674 -< _20
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _17)
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _22)
  _25 <- iPre 2.4593170263108868 -< _21
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _24)
  _27 <- iPre 1.8011166474674798 -< _26
  _28 <- FRP.Yampa.arr (+1) -< _27
  _29 <- FRP.Yampa.arr (+1) -< _28
  _30 <- iPre 1.6243916937890825 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr21 (+) -< {_2, _2}
    _4 <- arr21 (-) -< {_3, _1}
    _5 <- arr21 (+) -< {_2, _2}
    _6 <- arr21 (-) -< {_2, _1}
    _1 <- pre1 2.3610503747794276 -< _4
    _2 <- pre1 2.9282547582697362 -< _4

  _7 <- arr11 (+1) -< _4
  _8 <- arr11 (+1) -< _1
  _9 <- arr21 (*) -< {_5, _8}
  _10 <- arr21 (-) -< {_2, _6}
  _11 <- arr21 (*) -< {_8, _7}
  _12 <- pre1 2.4600520047158274 -< _0
  _13 <- arr21 (-) -< {_3, _3}
  _14 <- pre1 6.508747060652083e-2 -< _7
  _15 <- arr21 (-) -< {_10, _3}
  _16 <- arr21 (*) -< {_6, _8}
  _17 <- arr21 (*) -< {_9, _12}
  _18 <- arr21 (*) -< {_15, _11}
  _19 <- arr21 (*) -< {_18, _16}
  _20 <- pre1 7.484119078322501e-2 -< _19
  _21 <- pre1 2.734675890322731 -< _14
  _22 <- pre1 1.0392534188223674 -< _20
  _23 <- arr21 (+) -< {_13, _17}
  _24 <- arr21 (+) -< {_23, _22}
  _25 <- pre1 2.4593170263108868 -< _21
  _26 <- arr21 (+) -< {_25, _24}
  _27 <- pre1 1.8011166474674798 -< _26
  _28 <- arr11 (+1) -< _27
  _29 <- arr11 (+1) -< _28
  _30 <- pre1 1.6243916937890825 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6