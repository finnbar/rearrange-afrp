{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (+1) -< _1
    _3 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _1)
    _4 <- iPre 0.9125669183580019 -< _3
    _5 <- iPre 0.3302521411541887 -< _3
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _1)
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_5, _1)
    _8 <- iPre 1.9817940807021375 -< _2
    _9 <- iPre 1.2493716582313827 -< _0
    _10 <- FRP.Yampa.arr (+1) -< _4
    _11 <- FRP.Yampa.arr (+1) -< _7
    _12 <- iPre 2.662281922512792 -< _4
    _13 <- FRP.Yampa.arr (+1) -< _11
    _14 <- FRP.Yampa.arr (+1) -< _1
    _15 <- FRP.Yampa.arr (uncurry (-)) -< (_0, _2)
    _16 <- FRP.Yampa.arr (+1) -< _13
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _4)
    _18 <- iPre 0.7630064752556744 -< _9
    _19 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _18)
    _20 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _15)
    _21 <- iPre 2.7757563358925297 -< _12
    _22 <- FRP.Yampa.arr (uncurry (+)) -< (_20, _14)
    _23 <- FRP.Yampa.arr (+1) -< _22
    _24 <- iPre 1.8735712515372178 -< _21
    _25 <- FRP.Yampa.arr (+1) -< _23
    _26 <- iPre 2.2785714021606127 -< _25
    _27 <- FRP.Yampa.arr (uncurry (-)) -< (_17, _6)
    _28 <- iPre 1.3837118755713609 -< _26
    _29 <- FRP.Yampa.arr (+1) -< _28
    _30 <- FRP.Yampa.arr (uncurry (-)) -< (_29, _24)
    _31 <- FRP.Yampa.arr (+1) -< _30
    _32 <- FRP.Yampa.arr (+1) -< _19
    _33 <- FRP.Yampa.arr (+1) -< _27
    _34 <- FRP.Yampa.arr (uncurry (-)) -< (_33, _32)
    _35 <- FRP.Yampa.arr (uncurry (*)) -< (_31, _8)
    _36 <- iPre 2.9109997756667383 -< _35
    _37 <- FRP.Yampa.arr (+1) -< _36
    _38 <- FRP.Yampa.arr (+1) -< _34
    _39 <- FRP.Yampa.arr (+1) -< _37
    _40 <- FRP.Yampa.arr (+1) -< _39
    _1 <- iPre 2.4012261662783954 -< _40
  FRP.Yampa.returnA -< _38

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _1
    _3 <- arr21 (-) -< {_2, _1}
    _4 <- pre1 0.9125669183580019 -< _3
    _5 <- pre1 0.3302521411541887 -< _3
    _6 <- arr21 (+) -< {_3, _1}
    _7 <- arr21 (-) -< {_5, _1}
    _8 <- pre1 1.9817940807021375 -< _2
    _9 <- pre1 1.2493716582313827 -< _0
    _10 <- arr11 (+1) -< _4
    _11 <- arr11 (+1) -< _7
    _12 <- pre1 2.662281922512792 -< _4
    _13 <- arr11 (+1) -< _11
    _14 <- arr11 (+1) -< _1
    _15 <- arr21 (-) -< {_0, _2}
    _16 <- arr11 (+1) -< _13
    _17 <- arr21 (*) -< {_2, _4}
    _18 <- pre1 0.7630064752556744 -< _9
    _19 <- arr21 (*) -< {_16, _18}
    _20 <- arr21 (*) -< {_10, _15}
    _21 <- pre1 2.7757563358925297 -< _12
    _22 <- arr21 (+) -< {_20, _14}
    _23 <- arr11 (+1) -< _22
    _24 <- pre1 1.8735712515372178 -< _21
    _25 <- arr11 (+1) -< _23
    _26 <- pre1 2.2785714021606127 -< _25
    _27 <- arr21 (-) -< {_17, _6}
    _28 <- pre1 1.3837118755713609 -< _26
    _29 <- arr11 (+1) -< _28
    _30 <- arr21 (-) -< {_29, _24}
    _31 <- arr11 (+1) -< _30
    _32 <- arr11 (+1) -< _19
    _33 <- arr11 (+1) -< _27
    _34 <- arr21 (-) -< {_33, _32}
    _35 <- arr21 (*) -< {_31, _8}
    _36 <- pre1 2.9109997756667383 -< _35
    _37 <- arr11 (+1) -< _36
    _38 <- arr11 (+1) -< _34
    _39 <- arr11 (+1) -< _37
    _40 <- arr11 (+1) -< _39
    _1 <- pre1 2.4012261662783954 -< _40

  GenProc.GeneralisedArrow.returnA -< _38|]

codeLen :: Int
codeLen = 40
codeRecLen :: Int
codeRecLen = 40