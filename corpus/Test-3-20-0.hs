{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.9274276726627149 -< _0
  _2 <- iPre 0.5888861436638007 -< _1
  _3 <- FRP.Yampa.arr (+1) -< _1
  _4 <- iPre 1.6893186639276956 -< _1
  _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _0)
  _6 <- FRP.Yampa.arr (uncurry (*)) -< (_5, _1)
  _7 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _5)
  _8 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _4)
  _9 <- FRP.Yampa.arr (+1) -< _1
  _10 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _9)
  _11 <- iPre 2.720141216611859 -< _8
  _12 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _7)
  _13 <- FRP.Yampa.arr (+1) -< _2
  _14 <- iPre 0.9604665473029274 -< _12
  _15 <- iPre 1.2508820209333418 -< _13
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _16)
  _18 <- iPre 1.7042636781312555 -< _15
  _19 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _17)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_19, _10)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.9274276726627149 -< _0
  _2 <- pre1 0.5888861436638007 -< _1
  _3 <- arr11 (+1) -< _1
  _4 <- pre1 1.6893186639276956 -< _1
  _5 <- arr21 (+) -< {_4, _0}
  _6 <- arr21 (*) -< {_5, _1}
  _7 <- arr21 (+) -< {_0, _5}
  _8 <- arr21 (*) -< {_0, _4}
  _9 <- arr11 (+1) -< _1
  _10 <- arr21 (+) -< {_3, _9}
  _11 <- pre1 2.720141216611859 -< _8
  _12 <- arr21 (-) -< {_6, _7}
  _13 <- arr11 (+1) -< _2
  _14 <- pre1 0.9604665473029274 -< _12
  _15 <- pre1 1.2508820209333418 -< _13
  _16 <- arr11 (+1) -< _14
  _17 <- arr21 (-) -< {_11, _16}
  _18 <- pre1 1.7042636781312555 -< _15
  _19 <- arr21 (-) -< {_18, _17}
  _20 <- arr21 (*) -< {_19, _10}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0