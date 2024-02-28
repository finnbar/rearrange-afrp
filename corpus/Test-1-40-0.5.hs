{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  rec
    _4 <- iPre 0.9954120342502589 -< _1
    _5 <- FRP.Yampa.arr (+1) -< _2
    _6 <- iPre 0.4698320283082401 -< _3
    _7 <- FRP.Yampa.arr (+1) -< _3
    _8 <- FRP.Yampa.arr (+1) -< _0
    _9 <- iPre 1.4792851088588812 -< _6
    _10 <- iPre 0.8221735766880269 -< _3
    _11 <- FRP.Yampa.arr (+1) -< _2
    _12 <- FRP.Yampa.arr (+1) -< _2
    _13 <- FRP.Yampa.arr (+1) -< _9
    _14 <- FRP.Yampa.arr (+1) -< _1
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _11)
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _14)
    _17 <- FRP.Yampa.arr (+1) -< _14
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _5)
    _19 <- FRP.Yampa.arr (+1) -< _14
    _20 <- iPre 0.8151715143449629 -< _10
    _21 <- FRP.Yampa.arr (+1) -< _17
    _22 <- iPre 1.4909254078786125 -< _10
    _3 <- iPre 2.628552188501555 -< _22
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _7)
  _24 <- iPre 0.2455164172302314 -< _23
  _25 <- FRP.Yampa.arr (+1) -< _4
  _26 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _19)
  _27 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _20)
  _28 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _13)
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _28)
  _30 <- FRP.Yampa.arr (+1) -< _24
  _31 <- iPre 2.9381000837667015 -< _12
  _32 <- iPre 0.7033893382231389 -< _21
  _33 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _27)
  _34 <- iPre 2.4666809897414477 -< _32
  _35 <- FRP.Yampa.arr (uncurry (*)) -< (_31, _34)
  _36 <- FRP.Yampa.arr (uncurry (-)) -< (_33, _30)
  _37 <- iPre 0.912231924223939 -< _35
  _38 <- iPre 1.6890169660127785 -< _36
  _39 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _38)
  _40 <- FRP.Yampa.arr (uncurry (+)) -< (_39, _37)
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  rec
    _4 <- pre1 0.9954120342502589 -< _1
    _5 <- arr11 (+1) -< _2
    _6 <- pre1 0.4698320283082401 -< _3
    _7 <- arr11 (+1) -< _3
    _8 <- arr11 (+1) -< _0
    _9 <- pre1 1.4792851088588812 -< _6
    _10 <- pre1 0.8221735766880269 -< _3
    _11 <- arr11 (+1) -< _2
    _12 <- arr11 (+1) -< _2
    _13 <- arr11 (+1) -< _9
    _14 <- arr11 (+1) -< _1
    _15 <- arr21 (-) -< {_11, _11}
    _16 <- arr21 (+) -< {_3, _14}
    _17 <- arr11 (+1) -< _14
    _18 <- arr21 (*) -< {_0, _5}
    _19 <- arr11 (+1) -< _14
    _20 <- pre1 0.8151715143449629 -< _10
    _21 <- arr11 (+1) -< _17
    _22 <- pre1 1.4909254078786125 -< _10
    _3 <- pre1 2.628552188501555 -< _22

  _23 <- arr21 (-) -< {_8, _7}
  _24 <- pre1 0.2455164172302314 -< _23
  _25 <- arr11 (+1) -< _4
  _26 <- arr21 (-) -< {_16, _19}
  _27 <- arr21 (-) -< {_26, _20}
  _28 <- arr21 (*) -< {_15, _13}
  _29 <- arr21 (-) -< {_25, _28}
  _30 <- arr11 (+1) -< _24
  _31 <- pre1 2.9381000837667015 -< _12
  _32 <- pre1 0.7033893382231389 -< _21
  _33 <- arr21 (-) -< {_29, _27}
  _34 <- pre1 2.4666809897414477 -< _32
  _35 <- arr21 (*) -< {_31, _34}
  _36 <- arr21 (-) -< {_33, _30}
  _37 <- pre1 0.912231924223939 -< _35
  _38 <- pre1 1.6890169660127785 -< _36
  _39 <- arr21 (*) -< {_18, _38}
  _40 <- arr21 (+) -< {_39, _37}
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20