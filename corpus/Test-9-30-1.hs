{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _9 <- FRP.Yampa.arr (+1) -< _3
    _10 <- FRP.Yampa.arr (+1) -< _3
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_9, _2)
    _12 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _9)
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _0)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _9)
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _1)
    _16 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _5)
    _17 <- FRP.Yampa.arr (+1) -< _0
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _0)
    _19 <- FRP.Yampa.arr (+1) -< _12
    _20 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _13)
    _21 <- FRP.Yampa.arr (uncurry (-)) -< (_20, _2)
    _22 <- iPre 1.4923416460873722 -< _21
    _23 <- FRP.Yampa.arr (uncurry (-)) -< (_9, _3)
    _24 <- FRP.Yampa.arr (uncurry (+)) -< (_2, _19)
    _25 <- FRP.Yampa.arr (+1) -< _14
    _26 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _16)
    _27 <- FRP.Yampa.arr (+1) -< _10
    _28 <- iPre 1.4106225810379822 -< _23
    _29 <- FRP.Yampa.arr (uncurry (*)) -< (_22, _25)
    _30 <- FRP.Yampa.arr (+1) -< _4
    _1 <- iPre 0.2644293963652967 -< _30
    _2 <- iPre 2.936009058307011 -< _17
    _3 <- iPre 2.0689788313702517 -< _26
    _4 <- iPre 1.167626421801312 -< _6
    _5 <- iPre 2.413206837687718 -< _27
    _6 <- iPre 1.1004540189246785 -< _18
    _7 <- iPre 1.1750215786972988 -< _28
    _8 <- iPre 1.3285164392839064 -< _29
  FRP.Yampa.returnA -< _24

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _9 <- arr11 (+1) -< _3
    _10 <- arr11 (+1) -< _3
    _11 <- arr21 (*) -< {_9, _2}
    _12 <- arr21 (*) -< {_3, _9}
    _13 <- arr21 (-) -< {_11, _0}
    _14 <- arr21 (*) -< {_7, _9}
    _15 <- arr21 (+) -< {_9, _1}
    _16 <- arr21 (-) -< {_2, _5}
    _17 <- arr11 (+1) -< _0
    _18 <- arr21 (+) -< {_2, _0}
    _19 <- arr11 (+1) -< _12
    _20 <- arr21 (-) -< {_8, _13}
    _21 <- arr21 (-) -< {_20, _2}
    _22 <- pre1 1.4923416460873722 -< _21
    _23 <- arr21 (-) -< {_9, _3}
    _24 <- arr21 (+) -< {_2, _19}
    _25 <- arr11 (+1) -< _14
    _26 <- arr21 (+) -< {_15, _16}
    _27 <- arr11 (+1) -< _10
    _28 <- pre1 1.4106225810379822 -< _23
    _29 <- arr21 (*) -< {_22, _25}
    _30 <- arr11 (+1) -< _4
    _1 <- pre1 0.2644293963652967 -< _30
    _2 <- pre1 2.936009058307011 -< _17
    _3 <- pre1 2.0689788313702517 -< _26
    _4 <- pre1 1.167626421801312 -< _6
    _5 <- pre1 2.413206837687718 -< _27
    _6 <- pre1 1.1004540189246785 -< _18
    _7 <- pre1 1.1750215786972988 -< _28
    _8 <- pre1 1.3285164392839064 -< _29

  GenProc.GeneralisedArrow.returnA -< _24|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30