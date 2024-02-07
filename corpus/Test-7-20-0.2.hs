{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (+1) -< _1
  rec
    _5 <- FRP.Yampa.arr (+1) -< _2
    _6 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _4)
    _3 <- iPre 1.7666270337156857 -< _2
    _4 <- iPre 2.4713411233672926 -< _5
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _6)
  _8 <- iPre 2.7574004044411913 -< _7
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _3)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _0)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _10)
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _1)
  _13 <- iPre 2.8431322915069317 -< _1
  _14 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _12)
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _9)
  _16 <- iPre 1.184377090546081 -< _14
  _17 <- iPre 0.26152125077792765 -< _16
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _17)
  _19 <- iPre 0.9227475510103348 -< _18
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr11 (+1) -< _1
  rec
    _5 <- arr11 (+1) -< _2
    _6 <- arr21 (*) -< {_2, _4}
    _3 <- pre1 1.7666270337156857 -< _2
    _4 <- pre1 2.4713411233672926 -< _5

  _7 <- arr21 (+) -< {_3, _6}
  _8 <- pre1 2.7574004044411913 -< _7
  _9 <- arr21 (+) -< {_4, _3}
  _10 <- arr21 (*) -< {_8, _0}
  _11 <- arr21 (-) -< {_9, _10}
  _12 <- arr21 (+) -< {_11, _1}
  _13 <- pre1 2.8431322915069317 -< _1
  _14 <- arr21 (+) -< {_9, _12}
  _15 <- arr21 (+) -< {_1, _9}
  _16 <- pre1 1.184377090546081 -< _14
  _17 <- pre1 0.26152125077792765 -< _16
  _18 <- arr21 (-) -< {_13, _17}
  _19 <- pre1 0.9227475510103348 -< _18
  _20 <- arr21 (*) -< {_15, _19}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4