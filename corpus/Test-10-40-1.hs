{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _3)
    _5 <- FRP.Yampa.arr (+1) -< _4
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _1)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _6)
    _8 <- FRP.Yampa.arr (+1) -< _4
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- iPre 1.3063163995097304 -< _7
    _11 <- FRP.Yampa.arr (+1) -< _8
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _10)
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _8)
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _12)
    _15 <- iPre 0.5507319314506389 -< _14
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _13)
    _17 <- FRP.Yampa.arr (+1) -< _16
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _5)
    _19 <- FRP.Yampa.arr (+1) -< _5
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _3)
    _21 <- FRP.Yampa.arr (+1) -< _20
    _22 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _18)
    _23 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _9)
    _24 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _22)
    _25 <- FRP.Yampa.arr (+1) -< _24
    _26 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _19)
    _27 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _11)
    _28 <- FRP.Yampa.arr (+1) -< _27
    _29 <- iPre 8.349469999653948e-2 -< _27
    _30 <- FRP.Yampa.arr (+1) -< _11
    _31 <- iPre 5.9980337364487886e-2 -< _21
    _32 <- FRP.Yampa.arr (+1) -< _18
    _33 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _29)
    _34 <- FRP.Yampa.arr (+1) -< _33
    _35 <- FRP.Yampa.arr (uncurry (*)) -< (_31, _34)
    _36 <- FRP.Yampa.arr (+1) -< _28
    _37 <- FRP.Yampa.arr (uncurry (+)) -< (_36, _35)
    _38 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _37)
    _39 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _32)
    _40 <- iPre 1.9744468230716032 -< _38
    _1 <- iPre 0.9075419381004487 -< _40
  FRP.Yampa.returnA -< _39

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_0, _0}
    _3 <- arr21 (+) -< {_1, _2}
    _4 <- arr21 (*) -< {_2, _3}
    _5 <- arr11 (+1) -< _4
    _6 <- arr21 (-) -< {_3, _1}
    _7 <- arr21 (-) -< {_3, _6}
    _8 <- arr11 (+1) -< _4
    _9 <- arr11 (+1) -< _6
    _10 <- pre1 1.3063163995097304 -< _7
    _11 <- arr11 (+1) -< _8
    _12 <- arr21 (-) -< {_7, _10}
    _13 <- arr21 (-) -< {_1, _8}
    _14 <- arr21 (+) -< {_2, _12}
    _15 <- pre1 0.5507319314506389 -< _14
    _16 <- arr21 (-) -< {_9, _13}
    _17 <- arr11 (+1) -< _16
    _18 <- arr21 (*) -< {_15, _5}
    _19 <- arr11 (+1) -< _5
    _20 <- arr21 (-) -< {_17, _3}
    _21 <- arr11 (+1) -< _20
    _22 <- arr21 (-) -< {_0, _18}
    _23 <- arr21 (-) -< {_18, _9}
    _24 <- arr21 (*) -< {_23, _22}
    _25 <- arr11 (+1) -< _24
    _26 <- arr21 (*) -< {_21, _19}
    _27 <- arr21 (-) -< {_25, _11}
    _28 <- arr11 (+1) -< _27
    _29 <- pre1 8.349469999653948e-2 -< _27
    _30 <- arr11 (+1) -< _11
    _31 <- pre1 5.9980337364487886e-2 -< _21
    _32 <- arr11 (+1) -< _18
    _33 <- arr21 (*) -< {_25, _29}
    _34 <- arr11 (+1) -< _33
    _35 <- arr21 (*) -< {_31, _34}
    _36 <- arr11 (+1) -< _28
    _37 <- arr21 (+) -< {_36, _35}
    _38 <- arr21 (*) -< {_26, _37}
    _39 <- arr21 (+) -< {_30, _32}
    _40 <- pre1 1.9744468230716032 -< _38
    _1 <- pre1 0.9075419381004487 -< _40

  AFRP.returnA -< _39|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40