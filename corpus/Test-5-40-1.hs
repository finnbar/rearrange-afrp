{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (+1) -< _1
    _3 <- iPre 0.41776618435424373 -< _1
    _4 <- FRP.Yampa.arr (+1) -< _1
    _5 <- FRP.Yampa.arr (+1) -< _1
    _6 <- FRP.Yampa.arr (+1) -< _5
    _7 <- FRP.Yampa.arr (+1) -< _5
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _4)
    _9 <- iPre 2.244273133251255 -< _5
    _10 <- iPre 1.1909263906907757 -< _5
    _11 <- iPre 0.4752550865563062 -< _7
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _5)
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _2)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _2)
    _15 <- FRP.Yampa.arr (+1) -< _13
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _10)
    _17 <- iPre 2.7534537384385973 -< _1
    _18 <- iPre 1.5592909821960528 -< _5
    _19 <- iPre 2.84642043454766 -< _3
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _7)
    _21 <- iPre 2.9060134561176185 -< _2
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _9)
    _23 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _20)
    _24 <- FRP.Yampa.arr (uncurry (+)) -< (_21, _0)
    _25 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _12)
    _26 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _22)
    _27 <- iPre 2.388988758341808 -< _26
    _28 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _11)
    _29 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _19)
    _30 <- FRP.Yampa.arr (+1) -< _16
    _31 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _28)
    _32 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _31)
    _33 <- iPre 1.169941107128422 -< _14
    _34 <- FRP.Yampa.arr (+1) -< _32
    _35 <- iPre 2.019479408110581 -< _33
    _36 <- FRP.Yampa.arr (uncurry (+)) -< (_29, _30)
    _37 <- FRP.Yampa.arr (uncurry (+)) -< (_34, _35)
    _38 <- iPre 2.918833361789546 -< _37
    _39 <- iPre 0.803766231204637 -< _36
    _40 <- FRP.Yampa.arr (+1) -< _39
    _1 <- iPre 0.8434492184728577 -< _38
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _1
    _3 <- pre1 0.41776618435424373 -< _1
    _4 <- arr11 (+1) -< _1
    _5 <- arr11 (+1) -< _1
    _6 <- arr11 (+1) -< _5
    _7 <- arr11 (+1) -< _5
    _8 <- arr21 (*) -< {_2, _4}
    _9 <- pre1 2.244273133251255 -< _5
    _10 <- pre1 1.1909263906907757 -< _5
    _11 <- pre1 0.4752550865563062 -< _7
    _12 <- arr21 (*) -< {_1, _5}
    _13 <- arr21 (+) -< {_4, _2}
    _14 <- arr21 (*) -< {_10, _2}
    _15 <- arr11 (+1) -< _13
    _16 <- arr21 (-) -< {_1, _10}
    _17 <- pre1 2.7534537384385973 -< _1
    _18 <- pre1 1.5592909821960528 -< _5
    _19 <- pre1 2.84642043454766 -< _3
    _20 <- arr21 (-) -< {_17, _7}
    _21 <- pre1 2.9060134561176185 -< _2
    _22 <- arr21 (+) -< {_8, _9}
    _23 <- arr21 (*) -< {_15, _20}
    _24 <- arr21 (+) -< {_21, _0}
    _25 <- arr21 (+) -< {_6, _12}
    _26 <- arr21 (+) -< {_25, _22}
    _27 <- pre1 2.388988758341808 -< _26
    _28 <- arr21 (-) -< {_18, _11}
    _29 <- arr21 (*) -< {_23, _19}
    _30 <- arr11 (+1) -< _16
    _31 <- arr21 (+) -< {_27, _28}
    _32 <- arr21 (+) -< {_24, _31}
    _33 <- pre1 1.169941107128422 -< _14
    _34 <- arr11 (+1) -< _32
    _35 <- pre1 2.019479408110581 -< _33
    _36 <- arr21 (+) -< {_29, _30}
    _37 <- arr21 (+) -< {_34, _35}
    _38 <- pre1 2.918833361789546 -< _37
    _39 <- pre1 0.803766231204637 -< _36
    _40 <- arr11 (+1) -< _39
    _1 <- pre1 0.8434492184728577 -< _38

  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40