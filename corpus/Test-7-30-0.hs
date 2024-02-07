{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (+1) -< _0
  _2 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
  _3 <- iPre 1.2866801220396993 -< _0
  _4 <- iPre 2.7618165754017614 -< _2
  _5 <- FRP.Yampa.arr (+1) -< _3
  _6 <- iPre 2.34853089461679 -< _2
  _7 <- FRP.Yampa.arr (+1) -< _3
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _6)
  _9 <- FRP.Yampa.arr (+1) -< _3
  _10 <- iPre 1.1528398166836233 -< _7
  _11 <- FRP.Yampa.arr (+1) -< _9
  _12 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _1)
  _13 <- iPre 0.752125541523863 -< _10
  _14 <- iPre 1.591609598072294 -< _5
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _10)
  _16 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
  _17 <- iPre 0.5435446656534205 -< _11
  _18 <- iPre 1.8416480129129449 -< _8
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _4)
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _19)
  _21 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _20)
  _22 <- iPre 0.3087730665285832 -< _21
  _23 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _17)
  _24 <- FRP.Yampa.arr (uncurry (-)) -< (_23, _15)
  _25 <- iPre 1.2133801823369466 -< _14
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _25)
  _27 <- iPre 2.68328495531448 -< _26
  _28 <- iPre 0.6831952884980292 -< _27
  _29 <- FRP.Yampa.arr (+1) -< _24
  _30 <- FRP.Yampa.arr (uncurry (-)) -< (_28, _29)
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr11 (+1) -< _0
  _2 <- arr21 (+) -< {_0, _0}
  _3 <- pre1 1.2866801220396993 -< _0
  _4 <- pre1 2.7618165754017614 -< _2
  _5 <- arr11 (+1) -< _3
  _6 <- pre1 2.34853089461679 -< _2
  _7 <- arr11 (+1) -< _3
  _8 <- arr21 (-) -< {_3, _6}
  _9 <- arr11 (+1) -< _3
  _10 <- pre1 1.1528398166836233 -< _7
  _11 <- arr11 (+1) -< _9
  _12 <- arr21 (*) -< {_3, _1}
  _13 <- pre1 0.752125541523863 -< _10
  _14 <- pre1 1.591609598072294 -< _5
  _15 <- arr21 (-) -< {_0, _10}
  _16 <- arr21 (-) -< {_2, _1}
  _17 <- pre1 0.5435446656534205 -< _11
  _18 <- pre1 1.8416480129129449 -< _8
  _19 <- arr21 (*) -< {_12, _4}
  _20 <- arr21 (*) -< {_18, _19}
  _21 <- arr21 (*) -< {_13, _20}
  _22 <- pre1 0.3087730665285832 -< _21
  _23 <- arr21 (-) -< {_22, _17}
  _24 <- arr21 (-) -< {_23, _15}
  _25 <- pre1 1.2133801823369466 -< _14
  _26 <- arr21 (*) -< {_16, _25}
  _27 <- pre1 2.68328495531448 -< _26
  _28 <- pre1 0.6831952884980292 -< _27
  _29 <- arr11 (+1) -< _24
  _30 <- arr21 (-) -< {_28, _29}
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0