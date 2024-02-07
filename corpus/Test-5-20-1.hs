{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- FRP.Yampa.arr (+1) -< _1
    _4 <- iPre 2.788469803081491 -< _2
    _5 <- FRP.Yampa.arr (+1) -< _3
    _6 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _5)
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _0)
    _8 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _4)
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _2)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _8)
    _11 <- FRP.Yampa.arr (+1) -< _4
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _11)
    _13 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _7)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _13)
    _15 <- iPre 2.568322229998638 -< _14
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _11)
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_11, _5)
    _18 <- iPre 2.609567362131997 -< _16
    _19 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _10)
    _20 <- iPre 0.9769656801472663 -< _19
    _1 <- iPre 0.4251635795042138 -< _17
    _2 <- iPre 2.027901428368341 -< _15
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- arr11 (+1) -< _1
    _4 <- pre1 2.788469803081491 -< _2
    _5 <- arr11 (+1) -< _3
    _6 <- arr21 (-) -< {_3, _5}
    _7 <- arr21 (*) -< {_4, _0}
    _8 <- arr21 (-) -< {_6, _4}
    _9 <- arr21 (+) -< {_5, _2}
    _10 <- arr21 (*) -< {_1, _8}
    _11 <- arr11 (+1) -< _4
    _12 <- arr21 (*) -< {_9, _11}
    _13 <- arr21 (+) -< {_5, _7}
    _14 <- arr21 (*) -< {_12, _13}
    _15 <- pre1 2.568322229998638 -< _14
    _16 <- arr21 (-) -< {_9, _11}
    _17 <- arr21 (*) -< {_11, _5}
    _18 <- pre1 2.609567362131997 -< _16
    _19 <- arr21 (*) -< {_18, _10}
    _20 <- pre1 0.9769656801472663 -< _19
    _1 <- pre1 0.4251635795042138 -< _17
    _2 <- pre1 2.027901428368341 -< _15

  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20