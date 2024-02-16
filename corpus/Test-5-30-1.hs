{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- iPre 1.5085387627030964 -< _1
    _3 <- FRP.Yampa.arr (+1) -< _0
    _4 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _3)
    _5 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _4)
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_0, _0)
    _7 <- FRP.Yampa.arr (+1) -< _6
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _5)
    _9 <- FRP.Yampa.arr (uncurry (-)) -< (_8, _1)
    _10 <- iPre 1.5140268012333509 -< _9
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _7)
    _12 <- iPre 0.6036768851180416 -< _4
    _13 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _6)
    _14 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _9)
    _15 <- FRP.Yampa.arr (+1) -< _13
    _16 <- iPre 1.7209059306810617 -< _6
    _17 <- iPre 1.2880610033299205 -< _11
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _3)
    _19 <- iPre 2.6445040924607737 -< _14
    _20 <- iPre 1.8019175073245772 -< _19
    _21 <- FRP.Yampa.arr (+1) -< _20
    _22 <- FRP.Yampa.arr (+1) -< _19
    _23 <- FRP.Yampa.arr (uncurry (+)) -< (_12, _20)
    _24 <- FRP.Yampa.arr (+1) -< _21
    _25 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _22)
    _26 <- FRP.Yampa.arr (uncurry (*)) -< (_23, _15)
    _27 <- FRP.Yampa.arr (uncurry (-)) -< (_25, _26)
    _28 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _17)
    _29 <- iPre 0.8414732474543722 -< _24
    _30 <- FRP.Yampa.arr (uncurry (*)) -< (_29, _28)
    _1 <- iPre 2.6740565624631465 -< _27
  FRP.Yampa.returnA -< _30

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- pre1 1.5085387627030964 -< _1
    _3 <- arr11 (+1) -< _0
    _4 <- arr21 (-) -< {_2, _3}
    _5 <- arr21 (-) -< {_3, _4}
    _6 <- arr21 (+) -< {_0, _0}
    _7 <- arr11 (+1) -< _6
    _8 <- arr21 (+) -< {_7, _5}
    _9 <- arr21 (-) -< {_8, _1}
    _10 <- pre1 1.5140268012333509 -< _9
    _11 <- arr21 (*) -< {_2, _7}
    _12 <- pre1 0.6036768851180416 -< _4
    _13 <- arr21 (-) -< {_3, _6}
    _14 <- arr21 (-) -< {_2, _9}
    _15 <- arr11 (+1) -< _13
    _16 <- pre1 1.7209059306810617 -< _6
    _17 <- pre1 1.2880610033299205 -< _11
    _18 <- arr21 (*) -< {_10, _3}
    _19 <- pre1 2.6445040924607737 -< _14
    _20 <- pre1 1.8019175073245772 -< _19
    _21 <- arr11 (+1) -< _20
    _22 <- arr11 (+1) -< _19
    _23 <- arr21 (+) -< {_12, _20}
    _24 <- arr11 (+1) -< _21
    _25 <- arr21 (-) -< {_18, _22}
    _26 <- arr21 (*) -< {_23, _15}
    _27 <- arr21 (-) -< {_25, _26}
    _28 <- arr21 (+) -< {_16, _17}
    _29 <- pre1 0.8414732474543722 -< _24
    _30 <- arr21 (*) -< {_29, _28}
    _1 <- pre1 2.6740565624631465 -< _27

  GenProc.GeneralisedArrow.returnA -< _30|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30