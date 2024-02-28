{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- iPre 1.8634171544662217 -< _1
  _3 <- iPre 2.341417863442465 -< _2
  _4 <- iPre 0.40059593443879216 -< _2
  _5 <- iPre 1.0984382239410888 -< _0
  _6 <- FRP.Yampa.arr (+1) -< _4
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _2)
  _8 <- FRP.Yampa.arr (+1) -< _5
  _9 <- iPre 2.1735077498050637 -< _4
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _6)
  _11 <- FRP.Yampa.arr (+1) -< _7
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _10)
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _8)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _7)
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _14)
  _16 <- iPre 1.6901868340960435 -< _3
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _15)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _14)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _18)
  _20 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _19)
  _21 <- FRP.Yampa.arr (+1) -< _4
  _22 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _16)
  _23 <- iPre 1.8778151419032725 -< _1
  _24 <- iPre 2.17620815690003 -< _22
  _25 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _21)
  _26 <- FRP.Yampa.arr (+1) -< _13
  _27 <- iPre 0.9353355582020502 -< _2
  _28 <- iPre 0.6752683932849395 -< _26
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _24)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _28)
  _31 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _20)
  _32 <- FRP.Yampa.arr (uncurry (+)) -< (_31, _17)
  _33 <- FRP.Yampa.arr (+1) -< _32
  _34 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _27)
  _35 <- iPre 0.40490414361161264 -< _34
  _36 <- FRP.Yampa.arr (uncurry (+)) -< (_30, _35)
  _37 <- iPre 2.256976009162061 -< _33
  _38 <- iPre 0.888125917660617 -< _36
  _39 <- iPre 0.8181449736671004 -< _38
  _40 <- FRP.Yampa.arr (uncurry (+)) -< (_39, _37)
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- pre1 1.8634171544662217 -< _1
  _3 <- pre1 2.341417863442465 -< _2
  _4 <- pre1 0.40059593443879216 -< _2
  _5 <- pre1 1.0984382239410888 -< _0
  _6 <- arr11 (+1) -< _4
  _7 <- arr21 (+) -< {_1, _2}
  _8 <- arr11 (+1) -< _5
  _9 <- pre1 2.1735077498050637 -< _4
  _10 <- arr21 (+) -< {_4, _6}
  _11 <- arr11 (+1) -< _7
  _12 <- arr21 (*) -< {_11, _10}
  _13 <- arr21 (+) -< {_11, _8}
  _14 <- arr21 (*) -< {_13, _7}
  _15 <- arr21 (*) -< {_9, _14}
  _16 <- pre1 1.6901868340960435 -< _3
  _17 <- arr21 (+) -< {_5, _15}
  _18 <- arr21 (*) -< {_13, _14}
  _19 <- arr21 (+) -< {_5, _18}
  _20 <- arr21 (-) -< {_0, _19}
  _21 <- arr11 (+1) -< _4
  _22 <- arr21 (*) -< {_10, _16}
  _23 <- pre1 1.8778151419032725 -< _1
  _24 <- pre1 2.17620815690003 -< _22
  _25 <- arr21 (-) -< {_4, _21}
  _26 <- arr11 (+1) -< _13
  _27 <- pre1 0.9353355582020502 -< _2
  _28 <- pre1 0.6752683932849395 -< _26
  _29 <- arr21 (+) -< {_12, _24}
  _30 <- arr21 (*) -< {_29, _28}
  _31 <- arr21 (-) -< {_23, _20}
  _32 <- arr21 (+) -< {_31, _17}
  _33 <- arr11 (+1) -< _32
  _34 <- arr21 (*) -< {_25, _27}
  _35 <- pre1 0.40490414361161264 -< _34
  _36 <- arr21 (+) -< {_30, _35}
  _37 <- pre1 2.256976009162061 -< _33
  _38 <- pre1 0.888125917660617 -< _36
  _39 <- pre1 0.8181449736671004 -< _38
  _40 <- arr21 (+) -< {_39, _37}
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 0