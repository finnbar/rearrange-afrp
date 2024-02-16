{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  rec
    _3 <- FRP.Yampa.arr (+1) -< _1
    _4 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _3)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _4)
    _6 <- FRP.Yampa.arr (+1) -< _1
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _6)
    _8 <- FRP.Yampa.arr (+1) -< _5
    _9 <- FRP.Yampa.arr (+1) -< _8
    _10 <- FRP.Yampa.arr (+1) -< _7
    _11 <- iPre 0.4218646541421858 -< _7
    _12 <- FRP.Yampa.arr (+1) -< _4
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _5)
    _14 <- iPre 2.8478373505043444 -< _9
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _13)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _6)
    _17 <- FRP.Yampa.arr (+1) -< _15
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _16)
    _19 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _18)
    _20 <- iPre 0.4179177183606678 -< _14
    _21 <- iPre 1.1296622355643813 -< _20
    _2 <- iPre 6.518551358968325e-2 -< _21
  _22 <- FRP.Yampa.arr (+1) -< _10
  _23 <- FRP.Yampa.arr (+1) -< _3
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _15)
  _25 <- iPre 2.577227781368705 -< _19
  _26 <- iPre 1.3003509375909297 -< _24
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_25, _17)
  _28 <- FRP.Yampa.arr (+1) -< _15
  _29 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _27)
  _30 <- iPre 0.23455444966379738 -< _3
  _31 <- iPre 2.9556507466759645 -< _10
  _32 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _7)
  _33 <- FRP.Yampa.arr (uncurry (*)) -< (_24, _31)
  _34 <- FRP.Yampa.arr (uncurry (*)) -< (_30, _33)
  _35 <- iPre 1.962970501368834 -< _32
  _36 <- FRP.Yampa.arr (uncurry (-)) -< (_26, _34)
  _37 <- FRP.Yampa.arr (+1) -< _35
  _38 <- FRP.Yampa.arr (uncurry (*)) -< (_36, _29)
  _39 <- FRP.Yampa.arr (uncurry (-)) -< (_37, _28)
  _40 <- FRP.Yampa.arr (uncurry (*)) -< (_38, _39)
  FRP.Yampa.returnA -< _40

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  rec
    _3 <- arr11 (+1) -< _1
    _4 <- arr21 (*) -< {_2, _3}
    _5 <- arr21 (-) -< {_4, _4}
    _6 <- arr11 (+1) -< _1
    _7 <- arr21 (*) -< {_5, _6}
    _8 <- arr11 (+1) -< _5
    _9 <- arr11 (+1) -< _8
    _10 <- arr11 (+1) -< _7
    _11 <- pre1 0.4218646541421858 -< _7
    _12 <- arr11 (+1) -< _4
    _13 <- arr21 (-) -< {_1, _5}
    _14 <- pre1 2.8478373505043444 -< _9
    _15 <- arr21 (-) -< {_12, _13}
    _16 <- arr21 (*) -< {_9, _6}
    _17 <- arr11 (+1) -< _15
    _18 <- arr21 (+) -< {_11, _16}
    _19 <- arr21 (*) -< {_17, _18}
    _20 <- pre1 0.4179177183606678 -< _14
    _21 <- pre1 1.1296622355643813 -< _20
    _2 <- pre1 6.518551358968325e-2 -< _21

  _22 <- arr11 (+1) -< _10
  _23 <- arr11 (+1) -< _3
  _24 <- arr21 (*) -< {_22, _15}
  _25 <- pre1 2.577227781368705 -< _19
  _26 <- pre1 1.3003509375909297 -< _24
  _27 <- arr21 (+) -< {_25, _17}
  _28 <- arr11 (+1) -< _15
  _29 <- arr21 (+) -< {_23, _27}
  _30 <- pre1 0.23455444966379738 -< _3
  _31 <- pre1 2.9556507466759645 -< _10
  _32 <- arr21 (*) -< {_5, _7}
  _33 <- arr21 (*) -< {_24, _31}
  _34 <- arr21 (*) -< {_30, _33}
  _35 <- pre1 1.962970501368834 -< _32
  _36 <- arr21 (-) -< {_26, _34}
  _37 <- arr11 (+1) -< _35
  _38 <- arr21 (*) -< {_36, _29}
  _39 <- arr21 (-) -< {_37, _28}
  _40 <- arr21 (*) -< {_38, _39}
  GenProc.GeneralisedArrow.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 20