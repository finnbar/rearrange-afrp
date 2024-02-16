{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.2417740728580828 -< _0
  _2 <- iPre 2.7339083197024077 -< _0
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  rec
    _5 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _1)
    _6 <- iPre 2.7240481476451346 -< _1
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _4)
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _8)
    _4 <- iPre 5.078308161334712e-3 -< _0
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _2)
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _3)
  _12 <- FRP.Yampa.arr (+1) -< _4
  _13 <- iPre 0.2887333156628007 -< _10
  _14 <- FRP.Yampa.arr (+1) -< _13
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _13)
  _16 <- iPre 1.5423164190504295 -< _8
  _17 <- FRP.Yampa.arr (+1) -< _5
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _13)
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _6)
  _20 <- iPre 2.6994474251043354 -< _9
  _21 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _15)
  _22 <- FRP.Yampa.arr (+1) -< _14
  _23 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _16)
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _19)
  _25 <- FRP.Yampa.arr (+1) -< _24
  _26 <- FRP.Yampa.arr (+1) -< _18
  _27 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _23)
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_27, _26)
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _21)
  _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _25)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.2417740728580828 -< _0
  _2 <- pre1 2.7339083197024077 -< _0
  _3 <- arr21 (+) -< {_0, _0}
  rec
    _5 <- arr21 (*) -< {_1, _1}
    _6 <- pre1 2.7240481476451346 -< _1
    _7 <- arr21 (+) -< {_2, _0}
    _8 <- arr21 (-) -< {_1, _4}
    _9 <- arr21 (+) -< {_6, _8}
    _4 <- pre1 5.078308161334712e-3 -< _0

  _10 <- arr21 (+) -< {_7, _2}
  _11 <- arr21 (*) -< {_2, _3}
  _12 <- arr11 (+1) -< _4
  _13 <- pre1 0.2887333156628007 -< _10
  _14 <- arr11 (+1) -< _13
  _15 <- arr21 (-) -< {_3, _13}
  _16 <- pre1 1.5423164190504295 -< _8
  _17 <- arr11 (+1) -< _5
  _18 <- arr21 (-) -< {_9, _13}
  _19 <- arr21 (-) -< {_17, _6}
  _20 <- pre1 2.6994474251043354 -< _9
  _21 <- arr21 (-) -< {_11, _15}
  _22 <- arr11 (+1) -< _14
  _23 <- arr21 (+) -< {_12, _16}
  _24 <- arr21 (-) -< {_22, _19}
  _25 <- arr11 (+1) -< _24
  _26 <- arr11 (+1) -< _18
  _27 <- arr21 (-) -< {_20, _23}
  _28 <- arr21 (+) -< {_27, _26}
  _29 <- arr21 (-) -< {_28, _21}
  _30 <- arr21 (*) -< {_29, _25}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 6