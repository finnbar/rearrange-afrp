{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 1.4155562774062358 -< _1
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
    _4 <- iPre 0.14648241555215216 -< _3
    _1 <- iPre 1.8608747734487403 -< _2
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _4)
  _6 <- FRP.Yampa.arr (+1) -< _2
  _7 <- FRP.Yampa.arr (+1) -< _2
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _1)
  _9 <- iPre 2.0065575591051505 -< _1
  _10 <- iPre 1.1079462055820612 -< _1
  _11 <- iPre 2.8238166463790946 -< _6
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _10)
  _13 <- FRP.Yampa.arr (+1) -< _11
  _14 <- FRP.Yampa.arr (+1) -< _12
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _13)
  _16 <- FRP.Yampa.arr (+1) -< _8
  _17 <- iPre 1.2319409391302842 -< _15
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _16)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _9)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _18)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 1.4155562774062358 -< _1
    _3 <- arr21 (*) -< {_2, _1}
    _4 <- pre1 0.14648241555215216 -< _3
    _1 <- pre1 1.8608747734487403 -< _2

  _5 <- arr21 (+) -< {_0, _4}
  _6 <- arr11 (+1) -< _2
  _7 <- arr11 (+1) -< _2
  _8 <- arr21 (-) -< {_1, _1}
  _9 <- pre1 2.0065575591051505 -< _1
  _10 <- pre1 1.1079462055820612 -< _1
  _11 <- pre1 2.8238166463790946 -< _6
  _12 <- arr21 (-) -< {_7, _10}
  _13 <- arr11 (+1) -< _11
  _14 <- arr11 (+1) -< _12
  _15 <- arr21 (*) -< {_5, _13}
  _16 <- arr11 (+1) -< _8
  _17 <- pre1 1.2319409391302842 -< _15
  _18 <- arr21 (+) -< {_17, _16}
  _19 <- arr21 (+) -< {_14, _9}
  _20 <- arr21 (*) -< {_19, _18}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4