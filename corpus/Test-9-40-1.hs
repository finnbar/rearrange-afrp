{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _2)
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _4)
    _6 <- iPre 1.2632784541796076 -< _5
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _5)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _2)
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- iPre 0.32768813689406523 -< _7
    _11 <- iPre 1.201975070174404 -< _4
    _12 <- iPre 1.5179002882946084 -< _6
    _13 <- iPre 0.8744687607922438 -< _6
    _14 <- FRP.Yampa.arr (+1) -< _13
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _3)
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _8)
    _17 <- FRP.Yampa.arr (+1) -< _6
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _6)
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _13)
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _10)
    _21 <- iPre 2.081152328949628 -< _18
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _20)
    _23 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _21)
    _24 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _13)
    _25 <- FRP.Yampa.arr (+1) -< _15
    _26 <- FRP.Yampa.arr (uncurry (-)) -< (_19, _23)
    _27 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _24)
    _28 <- FRP.Yampa.arr (+1) -< _9
    _29 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _27)
    _30 <- iPre 0.20005918747715248 -< _11
    _31 <- FRP.Yampa.arr (uncurry (*)) -< (_28, _12)
    _32 <- iPre 0.115417221010118 -< _31
    _33 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _29)
    _34 <- FRP.Yampa.arr (+1) -< _33
    _35 <- FRP.Yampa.arr (+1) -< _25
    _36 <- iPre 2.2372521002199752 -< _34
    _37 <- FRP.Yampa.arr (uncurry (-)) -< (_32, _36)
    _38 <- iPre 1.2787201928209566 -< _35
    _39 <- FRP.Yampa.arr (+1) -< _38
    _40 <- FRP.Yampa.arr (+1) -< _37
    _1 <- iPre 2.3225252085440973 -< _39
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_1, _1}
    _3 <- arr21 (*) -< {_1, _2}
    _4 <- arr11 (+1) -< _1
    _5 <- arr21 (*) -< {_1, _4}
    _6 <- pre1 1.2632784541796076 -< _5
    _7 <- arr21 (*) -< {_2, _5}
    _8 <- arr21 (+) -< {_7, _2}
    _9 <- arr11 (+1) -< _6
    _10 <- pre1 0.32768813689406523 -< _7
    _11 <- pre1 1.201975070174404 -< _4
    _12 <- pre1 1.5179002882946084 -< _6
    _13 <- pre1 0.8744687607922438 -< _6
    _14 <- arr11 (+1) -< _13
    _15 <- arr21 (*) -< {_6, _3}
    _16 <- arr21 (-) -< {_7, _8}
    _17 <- arr11 (+1) -< _6
    _18 <- arr21 (+) -< {_17, _6}
    _19 <- arr21 (+) -< {_16, _13}
    _20 <- arr21 (+) -< {_16, _10}
    _21 <- pre1 2.081152328949628 -< _18
    _22 <- arr21 (+) -< {_14, _20}
    _23 <- arr21 (*) -< {_0, _21}
    _24 <- arr21 (-) -< {_6, _13}
    _25 <- arr11 (+1) -< _15
    _26 <- arr21 (-) -< {_19, _23}
    _27 <- arr21 (-) -< {_26, _24}
    _28 <- arr11 (+1) -< _9
    _29 <- arr21 (-) -< {_22, _27}
    _30 <- pre1 0.20005918747715248 -< _11
    _31 <- arr21 (*) -< {_28, _12}
    _32 <- pre1 0.115417221010118 -< _31
    _33 <- arr21 (+) -< {_30, _29}
    _34 <- arr11 (+1) -< _33
    _35 <- arr11 (+1) -< _25
    _36 <- pre1 2.2372521002199752 -< _34
    _37 <- arr21 (-) -< {_32, _36}
    _38 <- pre1 1.2787201928209566 -< _35
    _39 <- arr11 (+1) -< _38
    _40 <- arr11 (+1) -< _37
    _1 <- pre1 2.3225252085440973 -< _39

  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40