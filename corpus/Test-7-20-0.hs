{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.2788112509829461 -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _3)
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _4)
  _6 <- iPre 1.0972217793477619 -< _4
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _5)
  _8 <- FRP.Yampa.arr (+1) -< _4
  _9 <- iPre 1.4910745769122469 -< _8
  _10 <- FRP.Yampa.arr (+1) -< _8
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _4)
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _4)
  _13 <- iPre 1.3403435550310663 -< _6
  _14 <- iPre 1.9746069535316622 -< _4
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _13)
  _16 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _12)
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_15, _16)
  _18 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _17)
  _19 <- FRP.Yampa.arr (+1) -< _18
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.2788112509829461 -< _0
  _2 <- arr21 (*) -< {_1, _0}
  _3 <- arr21 (*) -< {_2, _1}
  _4 <- arr21 (+) -< {_2, _3}
  _5 <- arr21 (+) -< {_2, _4}
  _6 <- pre1 1.0972217793477619 -< _4
  _7 <- arr21 (+) -< {_5, _5}
  _8 <- arr11 (+1) -< _4
  _9 <- pre1 1.4910745769122469 -< _8
  _10 <- arr11 (+1) -< _8
  _11 <- arr21 (+) -< {_9, _4}
  _12 <- arr21 (*) -< {_11, _4}
  _13 <- pre1 1.3403435550310663 -< _6
  _14 <- pre1 1.9746069535316622 -< _4
  _15 <- arr21 (+) -< {_14, _13}
  _16 <- arr21 (*) -< {_7, _12}
  _17 <- arr21 (-) -< {_15, _16}
  _18 <- arr21 (+) -< {_10, _17}
  _19 <- arr11 (+1) -< _18
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0