{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- iPre 2.6493857189731553 -< _1
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _2)
  rec
    _7 <- FRP.Yampa.arr (+1) -< _1
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _4)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _8)
    _10 <- FRP.Yampa.arr (+1) -< _5
    _11 <- iPre 2.7689094929704194 -< _10
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _2)
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _7)
    _4 <- iPre 0.8088845610058385 -< _1
    _5 <- iPre 1.1376382810497316 -< _5
    _6 <- iPre 1.9313216250096068 -< _8
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _12)
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _10)
  _16 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _9)
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _15)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _13)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _18)
  _20 <- iPre 2.690126184590469 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- pre1 2.6493857189731553 -< _1
  _3 <- arr21 (*) -< {_0, _2}
  rec
    _7 <- arr11 (+1) -< _1
    _8 <- arr21 (*) -< {_2, _4}
    _9 <- arr21 (-) -< {_3, _8}
    _10 <- arr11 (+1) -< _5
    _11 <- pre1 2.7689094929704194 -< _10
    _12 <- arr21 (*) -< {_3, _2}
    _13 <- arr21 (-) -< {_10, _7}
    _4 <- pre1 0.8088845610058385 -< _1
    _5 <- pre1 1.1376382810497316 -< _5
    _6 <- pre1 1.9313216250096068 -< _8

  _14 <- arr21 (*) -< {_6, _12}
  _15 <- arr21 (-) -< {_12, _10}
  _16 <- arr21 (+) -< {_14, _9}
  _17 <- arr21 (+) -< {_16, _15}
  _18 <- arr21 (+) -< {_17, _13}
  _19 <- arr21 (+) -< {_11, _18}
  _20 <- pre1 2.690126184590469 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10