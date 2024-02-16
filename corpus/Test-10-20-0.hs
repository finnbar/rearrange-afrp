{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.549702368837073 -< _0
  _2 <- iPre 0.26646585312430626 -< _1
  _3 <- FRP.Yampa.arr (+1) -< _2
  _4 <- iPre 0.15152245150214982 -< _3
  _5 <- iPre 1.1674417265528163 -< _2
  _6 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _4)
  _7 <- FRP.Yampa.arr (+1) -< _6
  _8 <- iPre 0.11878693939457621 -< _7
  _9 <- iPre 1.5294167444485547 -< _8
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _5)
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _7)
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _11)
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _2)
  _14 <- FRP.Yampa.arr (+1) -< _13
  _15 <- iPre 0.9519365639650038 -< _5
  _16 <- FRP.Yampa.arr (+1) -< _6
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _12)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _17)
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _18)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _15)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.549702368837073 -< _0
  _2 <- pre1 0.26646585312430626 -< _1
  _3 <- arr11 (+1) -< _2
  _4 <- pre1 0.15152245150214982 -< _3
  _5 <- pre1 1.1674417265528163 -< _2
  _6 <- arr21 (+) -< {_5, _4}
  _7 <- arr11 (+1) -< _6
  _8 <- pre1 0.11878693939457621 -< _7
  _9 <- pre1 1.5294167444485547 -< _8
  _10 <- arr21 (+) -< {_9, _5}
  _11 <- arr21 (+) -< {_10, _7}
  _12 <- arr21 (-) -< {_9, _11}
  _13 <- arr21 (+) -< {_12, _2}
  _14 <- arr11 (+1) -< _13
  _15 <- pre1 0.9519365639650038 -< _5
  _16 <- arr11 (+1) -< _6
  _17 <- arr21 (-) -< {_16, _12}
  _18 <- arr21 (*) -< {_14, _17}
  _19 <- arr21 (*) -< {_5, _18}
  _20 <- arr21 (+) -< {_19, _15}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0