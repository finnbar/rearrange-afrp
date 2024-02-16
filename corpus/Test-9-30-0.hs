{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.4335572264972465 -< _0
  _2 <- FRP.Yampa.arr (+1) -< _1
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _0)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _3)
  _5 <- iPre 2.9641669795789425 -< _4
  _6 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _0)
  _7 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _3)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _2)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _8)
  _10 <- FRP.Yampa.arr (uncurry (-)) -< (_7, _4)
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _10)
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _11)
  _13 <- iPre 2.4300574396392056 -< _12
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _5)
  _15 <- FRP.Yampa.arr (+1) -< _0
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _14)
  _17 <- FRP.Yampa.arr (+1) -< _16
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _17)
  _19 <- iPre 0.20668785840662268 -< _18
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _4)
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_20, _13)
  _22 <- iPre 2.6713066178076597 -< _6
  _23 <- iPre 0.2753454964707487 -< _21
  _24 <- FRP.Yampa.arr (+1) -< _23
  _25 <- iPre 1.5798296082762244 -< _24
  _26 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _22)
  _27 <- iPre 2.969046933385326 -< _8
  _28 <- FRP.Yampa.arr (uncurry (+)) -< (_26, _27)
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _25)
  _30 <- iPre 0.16321876184968204 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.4335572264972465 -< _0
  _2 <- arr11 (+1) -< _1
  _3 <- arr21 (*) -< {_2, _0}
  _4 <- arr21 (+) -< {_3, _3}
  _5 <- pre1 2.9641669795789425 -< _4
  _6 <- arr21 (-) -< {_5, _0}
  _7 <- arr21 (-) -< {_6, _3}
  _8 <- arr21 (*) -< {_5, _2}
  _9 <- arr21 (+) -< {_7, _8}
  _10 <- arr21 (-) -< {_7, _4}
  _11 <- arr21 (+) -< {_0, _10}
  _12 <- arr21 (*) -< {_9, _11}
  _13 <- pre1 2.4300574396392056 -< _12
  _14 <- arr21 (-) -< {_13, _5}
  _15 <- arr11 (+1) -< _0
  _16 <- arr21 (-) -< {_10, _14}
  _17 <- arr11 (+1) -< _16
  _18 <- arr21 (-) -< {_15, _17}
  _19 <- pre1 0.20668785840662268 -< _18
  _20 <- arr21 (+) -< {_19, _4}
  _21 <- arr21 (*) -< {_20, _13}
  _22 <- pre1 2.6713066178076597 -< _6
  _23 <- pre1 0.2753454964707487 -< _21
  _24 <- arr11 (+1) -< _23
  _25 <- pre1 1.5798296082762244 -< _24
  _26 <- arr21 (+) -< {_10, _22}
  _27 <- pre1 2.969046933385326 -< _8
  _28 <- arr21 (+) -< {_26, _27}
  _29 <- arr21 (-) -< {_28, _25}
  _30 <- pre1 0.16321876184968204 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0