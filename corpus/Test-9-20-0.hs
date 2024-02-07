{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _3 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _2)
  _4 <- iPre 2.409566361097635 -< _3
  _5 <- iPre 1.7330766158950188 -< _2
  _6 <- iPre 2.7273574538535277 -< _0
  _7 <- FRP.Yampa.arr (+1) -< _4
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _6)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _5)
  _10 <- iPre 1.8147061333767487 -< _4
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _5)
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _11)
  _13 <- iPre 1.017871624734296 -< _5
  _14 <- FRP.Yampa.arr (+1) -< _5
  _15 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _10)
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _14)
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _15)
  _18 <- FRP.Yampa.arr (+1) -< _8
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_18, _12)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _17)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (+) -< {_0, _0}
  _2 <- arr21 (*) -< {_0, _1}
  _3 <- arr21 (+) -< {_2, _2}
  _4 <- pre1 2.409566361097635 -< _3
  _5 <- pre1 1.7330766158950188 -< _2
  _6 <- pre1 2.7273574538535277 -< _0
  _7 <- arr11 (+1) -< _4
  _8 <- arr21 (*) -< {_7, _6}
  _9 <- arr21 (*) -< {_7, _5}
  _10 <- pre1 1.8147061333767487 -< _4
  _11 <- arr21 (*) -< {_7, _5}
  _12 <- arr21 (-) -< {_0, _11}
  _13 <- pre1 1.017871624734296 -< _5
  _14 <- arr11 (+1) -< _5
  _15 <- arr21 (*) -< {_9, _10}
  _16 <- arr21 (-) -< {_13, _14}
  _17 <- arr21 (*) -< {_16, _15}
  _18 <- arr11 (+1) -< _8
  _19 <- arr21 (+) -< {_18, _12}
  _20 <- arr21 (+) -< {_19, _17}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0