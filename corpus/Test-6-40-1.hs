{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
    _4 <- FRP.Yampa.arr (+1) -< _2
    _5 <- iPre 2.2276581017099555 -< _4
    _6 <- FRP.Yampa.arr (+1) -< _3
    _7 <- iPre 2.783011619810353 -< _5
    _8 <- FRP.Yampa.arr (+1) -< _1
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _8)
    _10 <- FRP.Yampa.arr (+1) -< _9
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _7)
    _12 <- iPre 2.499074188891103 -< _8
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _6)
    _14 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _12)
    _15 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _2)
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _15)
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _15)
    _18 <- FRP.Yampa.arr (+1) -< _16
    _19 <- FRP.Yampa.arr (+1) -< _17
    _20 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _19)
    _21 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _20)
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _21)
    _23 <- FRP.Yampa.arr (+1) -< _9
    _24 <- FRP.Yampa.arr (+1) -< _12
    _25 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _23)
    _26 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _25)
    _27 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _24)
    _28 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _20)
    _29 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _28)
    _30 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _29)
    _31 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _30)
    _32 <- iPre 4.014364727215714e-2 -< _31
    _33 <- FRP.Yampa.arr (uncurry (+)) -< (_32, _20)
    _34 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _33)
    _35 <- iPre 0.5707191169121867 -< _19
    _36 <- iPre 0.9040290507161666 -< _18
    _37 <- iPre 0.4774926549448794 -< _36
    _38 <- FRP.Yampa.arr (uncurry (*)) -< (_35, _34)
    _39 <- FRP.Yampa.arr (uncurry (-)) -< (_38, _37)
    _40 <- iPre 1.1039063714464035 -< _35
    _1 <- iPre 2.4245211053178495 -< _40
  FRP.Yampa.returnA -< _39

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (+) -< {_0, _0}
    _3 <- arr21 (*) -< {_0, _0}
    _4 <- arr11 (+1) -< _2
    _5 <- pre1 2.2276581017099555 -< _4
    _6 <- arr11 (+1) -< _3
    _7 <- pre1 2.783011619810353 -< _5
    _8 <- arr11 (+1) -< _1
    _9 <- arr21 (+) -< {_7, _8}
    _10 <- arr11 (+1) -< _9
    _11 <- arr21 (+) -< {_10, _7}
    _12 <- pre1 2.499074188891103 -< _8
    _13 <- arr21 (*) -< {_11, _6}
    _14 <- arr21 (-) -< {_13, _12}
    _15 <- arr21 (*) -< {_9, _2}
    _16 <- arr21 (-) -< {_10, _15}
    _17 <- arr21 (*) -< {_8, _15}
    _18 <- arr11 (+1) -< _16
    _19 <- arr11 (+1) -< _17
    _20 <- arr21 (*) -< {_18, _19}
    _21 <- arr21 (*) -< {_9, _20}
    _22 <- arr21 (+) -< {_14, _21}
    _23 <- arr11 (+1) -< _9
    _24 <- arr11 (+1) -< _12
    _25 <- arr21 (*) -< {_5, _23}
    _26 <- arr21 (*) -< {_22, _25}
    _27 <- arr21 (+) -< {_12, _24}
    _28 <- arr21 (*) -< {_26, _20}
    _29 <- arr21 (+) -< {_27, _28}
    _30 <- arr21 (*) -< {_7, _29}
    _31 <- arr21 (-) -< {_14, _30}
    _32 <- pre1 4.014364727215714e-2 -< _31
    _33 <- arr21 (+) -< {_32, _20}
    _34 <- arr21 (*) -< {_15, _33}
    _35 <- pre1 0.5707191169121867 -< _19
    _36 <- pre1 0.9040290507161666 -< _18
    _37 <- pre1 0.4774926549448794 -< _36
    _38 <- arr21 (*) -< {_35, _34}
    _39 <- arr21 (-) -< {_38, _37}
    _40 <- pre1 1.1039063714464035 -< _35
    _1 <- pre1 2.4245211053178495 -< _40

  GenProc.GeneralisedArrow.returnA -< _39|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40