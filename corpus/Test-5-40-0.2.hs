{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _2)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _2)
  rec
    _6 <- iPre 1.1339326397846083 -< _3
    _7 <- FRP.Yampa.arr (+1) -< _5
    _8 <- FRP.Yampa.arr (+1) -< _6
    _9 <- FRP.Yampa.arr (+1) -< _6
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _5)
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _10)
    _12 <- iPre 1.4181049890319803 -< _2
    _5 <- iPre 2.4288557366513417 -< _5
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _4)
  _14 <- iPre 0.2903442287624833 -< _4
  _15 <- FRP.Yampa.arr (+1) -< _12
  _16 <- iPre 0.4655081093389521 -< _15
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _16)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _13)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _18)
  _20 <- iPre 2.355797851914044 -< _13
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _14)
  _22 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _7)
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _22)
  _24 <- FRP.Yampa.arr (uncurry (+)) -< (_23, _12)
  _25 <- FRP.Yampa.arr (uncurry (+)) -< (_24, _20)
  _26 <- iPre 1.8476979033969738 -< _14
  _27 <- iPre 2.5516172467411584 -< _19
  _28 <- FRP.Yampa.arr (+1) -< _21
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _28)
  _30 <- iPre 1.507797567605685 -< _25
  _31 <- iPre 1.9029565418076075 -< _26
  _32 <- FRP.Yampa.arr (uncurry (*)) -< (_30, _21)
  _33 <- FRP.Yampa.arr (+1) -< _27
  _34 <- iPre 0.6008131667158265 -< _31
  _35 <- FRP.Yampa.arr (+1) -< _29
  _36 <- FRP.Yampa.arr (uncurry (*)) -< (_33, _35)
  _37 <- iPre 1.616769224986179 -< _36
  _38 <- FRP.Yampa.arr (+1) -< _32
  _39 <- FRP.Yampa.arr (uncurry (+)) -< (_37, _38)
  _40 <- FRP.Yampa.arr (uncurry (+)) -< (_39, _34)
  FRP.Yampa.returnA -< _40

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  _3 <- arr21 (-) -< {_2, _2}
  _4 <- arr21 (+) -< {_3, _2}
  rec
    _6 <- pre1 1.1339326397846083 -< _3
    _7 <- arr11 (+1) -< _5
    _8 <- arr11 (+1) -< _6
    _9 <- arr11 (+1) -< _6
    _10 <- arr21 (*) -< {_3, _5}
    _11 <- arr21 (+) -< {_9, _10}
    _12 <- pre1 1.4181049890319803 -< _2
    _5 <- pre1 2.4288557366513417 -< _5

  _13 <- arr21 (+) -< {_6, _4}
  _14 <- pre1 0.2903442287624833 -< _4
  _15 <- arr11 (+1) -< _12
  _16 <- pre1 0.4655081093389521 -< _15
  _17 <- arr21 (*) -< {_6, _16}
  _18 <- arr21 (*) -< {_17, _13}
  _19 <- arr21 (+) -< {_8, _18}
  _20 <- pre1 2.355797851914044 -< _13
  _21 <- arr21 (*) -< {_2, _14}
  _22 <- arr21 (-) -< {_1, _7}
  _23 <- arr21 (-) -< {_11, _22}
  _24 <- arr21 (+) -< {_23, _12}
  _25 <- arr21 (+) -< {_24, _20}
  _26 <- pre1 1.8476979033969738 -< _14
  _27 <- pre1 2.5516172467411584 -< _19
  _28 <- arr11 (+1) -< _21
  _29 <- arr21 (-) -< {_6, _28}
  _30 <- pre1 1.507797567605685 -< _25
  _31 <- pre1 1.9029565418076075 -< _26
  _32 <- arr21 (*) -< {_30, _21}
  _33 <- arr11 (+1) -< _27
  _34 <- pre1 0.6008131667158265 -< _31
  _35 <- arr11 (+1) -< _29
  _36 <- arr21 (*) -< {_33, _35}
  _37 <- pre1 1.616769224986179 -< _36
  _38 <- arr11 (+1) -< _32
  _39 <- arr21 (+) -< {_37, _38}
  _40 <- arr21 (+) -< {_39, _34}
  AFRP.returnA -< _40|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 8