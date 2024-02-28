{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
    _3 <- FRP.Yampa.arr (+1) -< _0
    _4 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
    _5 <- iPre 1.3587811218141943 -< _0
    _6 <- FRP.Yampa.arr (+1) -< _2
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _1)
    _9 <- FRP.Yampa.arr (+1) -< _7
    _10 <- FRP.Yampa.arr (+1) -< _9
    _11 <- iPre 0.6323224379432159 -< _9
    _12 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _1)
    _13 <- FRP.Yampa.arr (+1) -< _2
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _12)
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _11)
    _16 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _3)
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _1)
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _2)
    _19 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _17)
    _20 <- iPre 0.1274565798054817 -< _10
    _21 <- FRP.Yampa.arr (+1) -< _4
    _22 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _20)
    _23 <- FRP.Yampa.arr (+1) -< _22
    _24 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _16)
    _25 <- iPre 2.509016591702708 -< _8
    _26 <- FRP.Yampa.arr (+1) -< _19
    _27 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _23)
    _28 <- iPre 2.6731772240466025 -< _25
    _29 <- FRP.Yampa.arr (uncurry (-)) -< (_24, _18)
    _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _5)
    _31 <- iPre 0.799565470811724 -< _27
    _32 <- FRP.Yampa.arr (+1) -< _31
    _33 <- FRP.Yampa.arr (+1) -< _13
    _34 <- FRP.Yampa.arr (+1) -< _28
    _35 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _30)
    _36 <- FRP.Yampa.arr (uncurry (+)) -< (_32, _34)
    _37 <- iPre 0.6926735088808296 -< _33
    _38 <- FRP.Yampa.arr (uncurry (-)) -< (_36, _37)
    _39 <- iPre 0.7659824440872702 -< _38
    _40 <- FRP.Yampa.arr (+1) -< _39
    _1 <- iPre 3.0155676809640686 -< _35
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (+) -< {_1, _1}
    _3 <- arr11 (+1) -< _0
    _4 <- arr21 (+) -< {_1, _2}
    _5 <- pre1 1.3587811218141943 -< _0
    _6 <- arr11 (+1) -< _2
    _7 <- arr21 (*) -< {_3, _2}
    _8 <- arr21 (-) -< {_7, _1}
    _9 <- arr11 (+1) -< _7
    _10 <- arr11 (+1) -< _9
    _11 <- pre1 0.6323224379432159 -< _9
    _12 <- arr21 (+) -< {_0, _1}
    _13 <- arr11 (+1) -< _2
    _14 <- arr21 (*) -< {_9, _12}
    _15 <- arr21 (-) -< {_1, _11}
    _16 <- arr21 (+) -< {_7, _3}
    _17 <- arr21 (+) -< {_3, _1}
    _18 <- arr21 (*) -< {_11, _2}
    _19 <- arr21 (*) -< {_15, _17}
    _20 <- pre1 0.1274565798054817 -< _10
    _21 <- arr11 (+1) -< _4
    _22 <- arr21 (*) -< {_21, _20}
    _23 <- arr11 (+1) -< _22
    _24 <- arr21 (+) -< {_14, _16}
    _25 <- pre1 2.509016591702708 -< _8
    _26 <- arr11 (+1) -< _19
    _27 <- arr21 (*) -< {_26, _23}
    _28 <- pre1 2.6731772240466025 -< _25
    _29 <- arr21 (-) -< {_24, _18}
    _30 <- arr21 (*) -< {_29, _5}
    _31 <- pre1 0.799565470811724 -< _27
    _32 <- arr11 (+1) -< _31
    _33 <- arr11 (+1) -< _13
    _34 <- arr11 (+1) -< _28
    _35 <- arr21 (+) -< {_6, _30}
    _36 <- arr21 (+) -< {_32, _34}
    _37 <- pre1 0.6926735088808296 -< _33
    _38 <- arr21 (-) -< {_36, _37}
    _39 <- pre1 0.7659824440872702 -< _38
    _40 <- arr11 (+1) -< _39
    _1 <- pre1 3.0155676809640686 -< _35

  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40