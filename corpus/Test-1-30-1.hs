{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _3)
    _5 <- iPre 1.6669506188512244 -< _3
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _2)
    _7 <- FRP.Yampa.arr (+1) -< _6
    _8 <- FRP.Yampa.arr (+1) -< _7
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- iPre 2.9299652490069303 -< _8
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _6)
    _12 <- iPre 0.6315403854019807 -< _1
    _13 <- FRP.Yampa.arr (+1) -< _3
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _11)
    _15 <- iPre 2.8376292859761785 -< _10
    _16 <- iPre 0.537598379726257 -< _11
    _17 <- iPre 0.38462046607621636 -< _8
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _9)
    _20 <- FRP.Yampa.arr (+1) -< _14
    _21 <- FRP.Yampa.arr (+1) -< _15
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _0)
    _23 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _5)
    _24 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _16)
    _25 <- FRP.Yampa.arr (+1) -< _12
    _26 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _25)
    _27 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _13)
    _28 <- FRP.Yampa.arr (+1) -< _26
    _29 <- iPre 2.588625324802768 -< _28
    _30 <- iPre 1.9830893085244465 -< _29
    _1 <- iPre 0.13095084609783286 -< _27
    _2 <- iPre 2.8126711553482293 -< _24
    _3 <- iPre 1.9862494392706391 -< _30
  FRP.Yampa.returnA -< _19

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr21 (*) -< {_3, _3}
    _5 <- pre1 1.6669506188512244 -< _3
    _6 <- arr21 (+) -< {_4, _2}
    _7 <- arr11 (+1) -< _6
    _8 <- arr11 (+1) -< _7
    _9 <- arr11 (+1) -< _6
    _10 <- pre1 2.9299652490069303 -< _8
    _11 <- arr21 (*) -< {_9, _6}
    _12 <- pre1 0.6315403854019807 -< _1
    _13 <- arr11 (+1) -< _3
    _14 <- arr21 (*) -< {_8, _11}
    _15 <- pre1 2.8376292859761785 -< _10
    _16 <- pre1 0.537598379726257 -< _11
    _17 <- pre1 0.38462046607621636 -< _8
    _18 <- arr21 (+) -< {_1, _1}
    _19 <- arr21 (-) -< {_18, _9}
    _20 <- arr11 (+1) -< _14
    _21 <- arr11 (+1) -< _15
    _22 <- arr21 (+) -< {_21, _0}
    _23 <- arr21 (*) -< {_20, _5}
    _24 <- arr21 (-) -< {_23, _16}
    _25 <- arr11 (+1) -< _12
    _26 <- arr21 (-) -< {_17, _25}
    _27 <- arr21 (*) -< {_22, _13}
    _28 <- arr11 (+1) -< _26
    _29 <- pre1 2.588625324802768 -< _28
    _30 <- pre1 1.9830893085244465 -< _29
    _1 <- pre1 0.13095084609783286 -< _27
    _2 <- pre1 2.8126711553482293 -< _24
    _3 <- pre1 1.9862494392706391 -< _30

  GenProc.GeneralisedArrow.returnA -< _19|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30