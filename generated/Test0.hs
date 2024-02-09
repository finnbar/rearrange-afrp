{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 1.1456207770424232 -< _1
    _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
    _4 <- FRP.Yampa.arr (+1) -< _3
    _5 <- iPre 2.2878761909338206 -< _4
    _6 <- FRP.Yampa.arr (+1) -< _1
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _3)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _1)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _8)
    _10 <- FRP.Yampa.arr (+1) -< _5
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _10)
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _8)
    _13 <- FRP.Yampa.arr (+1) -< _9
    _14 <- iPre 2.6165662536078322 -< _9
    _15 <- FRP.Yampa.arr (+1) -< _12
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _2)
    _17 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _5)
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _15)
    _19 <- FRP.Yampa.arr (+1) -< _17
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _13)
    _21 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _10)
    _22 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _4)
    _23 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _14)
    _24 <- iPre 2.770256008443274 -< _19
    _25 <- iPre 0.24609487200275013 -< _23
    _26 <- FRP.Yampa.arr (+1) -< _21
    _27 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _20)
    _28 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _22)
    _29 <- iPre 2.4163870158051792 -< _28
    _30 <- FRP.Yampa.arr (+1) -< _27
    _31 <- iPre 0.1924941408279831 -< _29
    _32 <- iPre 2.2459508358743547 -< _25
    _33 <- iPre 2.544252847900099 -< _31
    _34 <- iPre 0.4507535822925564 -< _33
    _35 <- FRP.Yampa.arr (uncurry (-)) -< (_30, _32)
    _36 <- FRP.Yampa.arr (+1) -< _26
    _37 <- FRP.Yampa.arr (+1) -< _34
    _38 <- iPre 0.40816003089329694 -< _37
    _39 <- FRP.Yampa.arr (+1) -< _35
    _40 <- FRP.Yampa.arr (uncurry (-)) -< (_36, _38)
    _1 <- iPre 1.926477619204536 -< _40
  FRP.Yampa.returnA -< _39

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 1.1456207770424232 -< _1
    _3 <- arr21 (+) -< {_2, _0}
    _4 <- arr11 (+1) -< _3
    _5 <- pre1 2.2878761909338206 -< _4
    _6 <- arr11 (+1) -< _1
    _7 <- arr21 (+) -< {_1, _3}
    _8 <- arr21 (+) -< {_7, _1}
    _9 <- arr21 (-) -< {_6, _8}
    _10 <- arr11 (+1) -< _5
    _11 <- arr21 (+) -< {_3, _10}
    _12 <- arr21 (+) -< {_11, _8}
    _13 <- arr11 (+1) -< _9
    _14 <- pre1 2.6165662536078322 -< _9
    _15 <- arr11 (+1) -< _12
    _16 <- arr21 (+) -< {_15, _2}
    _17 <- arr21 (-) -< {_9, _5}
    _18 <- arr21 (+) -< {_9, _15}
    _19 <- arr11 (+1) -< _17
    _20 <- arr21 (-) -< {_10, _13}
    _21 <- arr21 (-) -< {_1, _10}
    _22 <- arr21 (-) -< {_6, _4}
    _23 <- arr21 (+) -< {_16, _14}
    _24 <- pre1 2.770256008443274 -< _19
    _25 <- pre1 0.24609487200275013 -< _23
    _26 <- arr11 (+1) -< _21
    _27 <- arr21 (+) -< {_18, _20}
    _28 <- arr21 (-) -< {_24, _22}
    _29 <- pre1 2.4163870158051792 -< _28
    _30 <- arr11 (+1) -< _27
    _31 <- pre1 0.1924941408279831 -< _29
    _32 <- pre1 2.2459508358743547 -< _25
    _33 <- pre1 2.544252847900099 -< _31
    _34 <- pre1 0.4507535822925564 -< _33
    _35 <- arr21 (-) -< {_30, _32}
    _36 <- arr11 (+1) -< _26
    _37 <- arr11 (+1) -< _34
    _38 <- pre1 0.40816003089329694 -< _37
    _39 <- arr11 (+1) -< _35
    _40 <- arr21 (-) -< {_36, _38}
    _1 <- pre1 1.926477619204536 -< _40

  GenProc.GeneralisedArrow.returnA -< _39|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40