{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _0)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _3)
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _3)
  rec
    _7 <- iPre 1.1183764048939995 -< _4
    _8 <- FRP.Yampa.arr (+1) -< _1
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _0)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _5)
    _11 <- iPre 0.3121378083905929 -< _10
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _3)
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _3)
    _14 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _8)
    _15 <- iPre 2.2032662142357466 -< _10
    _16 <- FRP.Yampa.arr (+1) -< _3
    _17 <- FRP.Yampa.arr (+1) -< _7
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _15)
    _19 <- FRP.Yampa.arr (+1) -< _16
    _20 <- iPre 2.578505826364899 -< _18
    _21 <- iPre 1.866407071156557 -< _16
    _22 <- FRP.Yampa.arr (+1) -< _13
    _23 <- FRP.Yampa.arr (+1) -< _14
    _24 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _8)
    _25 <- iPre 1.075340442944158 -< _6
    _6 <- iPre 1.4109096251035953 -< _23
  _26 <- iPre 2.244797281653977 -< _0
  _27 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _18)
  _28 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _22)
  _29 <- iPre 0.772369889461763 -< _24
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_27, _29)
  _31 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _19)
  _32 <- iPre 2.897653703246615 -< _20
  _33 <- iPre 2.9969746040190848 -< _32
  _34 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _31)
  _35 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _33)
  _36 <- FRP.Yampa.arr (+1) -< _34
  _37 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _36)
  _38 <- FRP.Yampa.arr (uncurry (+)) -< (_37, _35)
  _39 <- iPre 2.941147668316617 -< _38
  _40 <- iPre 2.22373362599309 -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _0}
  _3 <- arr21 (-) -< {_2, _0}
  _4 <- arr21 (+) -< {_0, _3}
  _5 <- arr21 (-) -< {_1, _3}
  rec
    _7 <- pre1 1.1183764048939995 -< _4
    _8 <- arr11 (+1) -< _1
    _9 <- arr21 (-) -< {_4, _0}
    _10 <- arr21 (+) -< {_3, _5}
    _11 <- pre1 0.3121378083905929 -< _10
    _12 <- arr21 (-) -< {_11, _3}
    _13 <- arr21 (+) -< {_11, _3}
    _14 <- arr21 (-) -< {_3, _8}
    _15 <- pre1 2.2032662142357466 -< _10
    _16 <- arr11 (+1) -< _3
    _17 <- arr11 (+1) -< _7
    _18 <- arr21 (+) -< {_9, _15}
    _19 <- arr11 (+1) -< _16
    _20 <- pre1 2.578505826364899 -< _18
    _21 <- pre1 1.866407071156557 -< _16
    _22 <- arr11 (+1) -< _13
    _23 <- arr11 (+1) -< _14
    _24 <- arr21 (+) -< {_21, _8}
    _25 <- pre1 1.075340442944158 -< _6
    _6 <- pre1 1.4109096251035953 -< _23

  _26 <- pre1 2.244797281653977 -< _0
  _27 <- arr21 (*) -< {_12, _18}
  _28 <- arr21 (-) -< {_25, _22}
  _29 <- pre1 0.772369889461763 -< _24
  _30 <- arr21 (*) -< {_27, _29}
  _31 <- arr21 (-) -< {_28, _19}
  _32 <- pre1 2.897653703246615 -< _20
  _33 <- pre1 2.9969746040190848 -< _32
  _34 <- arr21 (+) -< {_26, _31}
  _35 <- arr21 (+) -< {_30, _33}
  _36 <- arr11 (+1) -< _34
  _37 <- arr21 (+) -< {_17, _36}
  _38 <- arr21 (+) -< {_37, _35}
  _39 <- pre1 2.941147668316617 -< _38
  _40 <- pre1 2.22373362599309 -< _39
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20