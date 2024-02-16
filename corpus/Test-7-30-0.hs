{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _0)
  _2 <- iPre 2.939778278859012 -< _0
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _4 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _3)
  _5 <- iPre 1.4120918141583212 -< _3
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _5)
  _7 <- iPre 0.4183393415401865 -< _1
  _8 <- FRP.Yampa.arr (+1) -< _6
  _9 <- iPre 0.5996939863092335 -< _7
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _2)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
  _12 <- iPre 2.943104530016845 -< _9
  _13 <- FRP.Yampa.arr (+1) -< _11
  _14 <- FRP.Yampa.arr (+1) -< _10
  _15 <- FRP.Yampa.arr (+1) -< _4
  _16 <- iPre 0.7547305981116651 -< _14
  _17 <- iPre 9.154371883011561e-2 -< _4
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _16)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _7)
  _20 <- iPre 1.0585984613525916 -< _3
  _21 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _18)
  _22 <- FRP.Yampa.arr (+1) -< _19
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _20)
  _24 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _23)
  _25 <- FRP.Yampa.arr (uncurry (*)) -< (_21, _17)
  _26 <- FRP.Yampa.arr (+1) -< _22
  _27 <- iPre 2.818784152040236 -< _26
  _28 <- iPre 2.0605767927070855 -< _27
  _29 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _24)
  _30 <- FRP.Yampa.arr (uncurry (+)) -< (_29, _28)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (-) -< {_0, _0}
  _2 <- pre1 2.939778278859012 -< _0
  _3 <- arr21 (*) -< {_1, _0}
  _4 <- arr21 (*) -< {_2, _3}
  _5 <- pre1 1.4120918141583212 -< _3
  _6 <- arr21 (+) -< {_2, _5}
  _7 <- pre1 0.4183393415401865 -< _1
  _8 <- arr11 (+1) -< _6
  _9 <- pre1 0.5996939863092335 -< _7
  _10 <- arr21 (-) -< {_4, _2}
  _11 <- arr21 (-) -< {_2, _1}
  _12 <- pre1 2.943104530016845 -< _9
  _13 <- arr11 (+1) -< _11
  _14 <- arr11 (+1) -< _10
  _15 <- arr11 (+1) -< _4
  _16 <- pre1 0.7547305981116651 -< _14
  _17 <- pre1 9.154371883011561e-2 -< _4
  _18 <- arr21 (*) -< {_11, _16}
  _19 <- arr21 (*) -< {_12, _7}
  _20 <- pre1 1.0585984613525916 -< _3
  _21 <- arr21 (+) -< {_13, _18}
  _22 <- arr11 (+1) -< _19
  _23 <- arr21 (-) -< {_8, _20}
  _24 <- arr21 (*) -< {_15, _23}
  _25 <- arr21 (*) -< {_21, _17}
  _26 <- arr11 (+1) -< _22
  _27 <- pre1 2.818784152040236 -< _26
  _28 <- pre1 2.0605767927070855 -< _27
  _29 <- arr21 (*) -< {_25, _24}
  _30 <- arr21 (+) -< {_29, _28}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0