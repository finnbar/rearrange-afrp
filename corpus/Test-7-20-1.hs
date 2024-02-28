{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
    _3 <- iPre 1.2588330286501712 -< _2
    _4 <- iPre 0.755254381908136 -< _3
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _3)
    _6 <- FRP.Yampa.arr (+1) -< _1
    _7 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _4)
    _8 <- iPre 2.5149990012002004 -< _7
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _6)
    _10 <- iPre 0.4442534055026011 -< _5
    _11 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _2)
    _12 <- iPre 1.2026904114507415 -< _8
    _13 <- FRP.Yampa.arr (+1) -< _10
    _14 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _1)
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_11, _14)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_15, _9)
    _17 <- iPre 2.4715991308731513 -< _14
    _18 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _16)
    _19 <- FRP.Yampa.arr (uncurry (-)) -< (_11, _17)
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_19, _13)
    _1 <- iPre 1.09261370517577 -< _18
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr21 (*) -< {_1, _0}
    _3 <- pre1 1.2588330286501712 -< _2
    _4 <- pre1 0.755254381908136 -< _3
    _5 <- arr21 (+) -< {_4, _3}
    _6 <- arr11 (+1) -< _1
    _7 <- arr21 (*) -< {_0, _4}
    _8 <- pre1 2.5149990012002004 -< _7
    _9 <- arr21 (+) -< {_5, _6}
    _10 <- pre1 0.4442534055026011 -< _5
    _11 <- arr21 (+) -< {_0, _2}
    _12 <- pre1 1.2026904114507415 -< _8
    _13 <- arr11 (+1) -< _10
    _14 <- arr21 (+) -< {_12, _1}
    _15 <- arr21 (+) -< {_11, _14}
    _16 <- arr21 (*) -< {_15, _9}
    _17 <- pre1 2.4715991308731513 -< _14
    _18 <- arr21 (+) -< {_6, _16}
    _19 <- arr21 (-) -< {_11, _17}
    _20 <- arr21 (+) -< {_19, _13}
    _1 <- pre1 1.09261370517577 -< _18

  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20