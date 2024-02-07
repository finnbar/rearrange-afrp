{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _0)
  _2 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _2)
  _4 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _3)
  _5 <- FRP.Yampa.arr (uncurry (-)) -< (_1, _4)
  _6 <- iPre 1.9105420518572438 -< _5
  _7 <- iPre 2.958447928687009 -< _5
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _6)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _8)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _0)
  _11 <- FRP.Yampa.arr (uncurry (-)) -< (_10, _9)
  _12 <- iPre 0.5669039361581112 -< _11
  _13 <- iPre 0.33659434005092626 -< _12
  _14 <- FRP.Yampa.arr (uncurry (-)) -< (_13, _2)
  _15 <- iPre 2.427036364045918 -< _11
  _16 <- FRP.Yampa.arr (+1) -< _15
  _17 <- FRP.Yampa.arr (uncurry (-)) -< (_16, _2)
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _14)
  _19 <- iPre 1.3000284786146978 -< _7
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _2)
  _21 <- FRP.Yampa.arr (+1) -< _19
  _22 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _21)
  _23 <- iPre 1.8579839351379226 -< _17
  _24 <- iPre 0.2031873010531009 -< _23
  _25 <- iPre 1.489597988169412 -< _22
  _26 <- FRP.Yampa.arr (uncurry (*)) -< (_25, _24)
  _27 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _26)
  _28 <- iPre 2.0643561298793505 -< _27
  _29 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _28)
  _30 <- iPre 1.2960906800566114 -< _29
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- arr21 (*) -< {_0, _0}
  _2 <- arr21 (-) -< {_1, _0}
  _3 <- arr21 (-) -< {_1, _2}
  _4 <- arr21 (+) -< {_3, _3}
  _5 <- arr21 (-) -< {_1, _4}
  _6 <- pre1 1.9105420518572438 -< _5
  _7 <- pre1 2.958447928687009 -< _5
  _8 <- arr21 (+) -< {_3, _6}
  _9 <- arr21 (*) -< {_2, _8}
  _10 <- arr21 (*) -< {_7, _0}
  _11 <- arr21 (-) -< {_10, _9}
  _12 <- pre1 0.5669039361581112 -< _11
  _13 <- pre1 0.33659434005092626 -< _12
  _14 <- arr21 (-) -< {_13, _2}
  _15 <- pre1 2.427036364045918 -< _11
  _16 <- arr11 (+1) -< _15
  _17 <- arr21 (-) -< {_16, _2}
  _18 <- arr21 (*) -< {_15, _14}
  _19 <- pre1 1.3000284786146978 -< _7
  _20 <- arr21 (+) -< {_14, _2}
  _21 <- arr11 (+1) -< _19
  _22 <- arr21 (+) -< {_2, _21}
  _23 <- pre1 1.8579839351379226 -< _17
  _24 <- pre1 0.2031873010531009 -< _23
  _25 <- pre1 1.489597988169412 -< _22
  _26 <- arr21 (*) -< {_25, _24}
  _27 <- arr21 (+) -< {_20, _26}
  _28 <- pre1 2.0643561298793505 -< _27
  _29 <- arr21 (-) -< {_18, _28}
  _30 <- pre1 1.2960906800566114 -< _29
  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 0