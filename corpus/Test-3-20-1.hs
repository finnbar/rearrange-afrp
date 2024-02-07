{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _4 <- FRP.Yampa.arr (+1) -< _3
    _5 <- iPre 2.540063260732449 -< _3
    _6 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _1)
    _7 <- iPre 0.782939052624729 -< _3
    _8 <- iPre 2.4979211944661213 -< _7
    _9 <- iPre 0.15900688111974254 -< _7
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _3)
    _11 <- FRP.Yampa.arr (+1) -< _1
    _12 <- FRP.Yampa.arr (+1) -< _2
    _13 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _11)
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _10)
    _15 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _4)
    _16 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _11)
    _17 <- iPre 1.3629851664971964 -< _0
    _18 <- iPre 0.20343964196341788 -< _17
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_15, _18)
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _13)
    _1 <- iPre 1.6478318552861126 -< _20
    _2 <- iPre 1.1153464279445746 -< _9
    _3 <- iPre 1.8679849718958177 -< _19
  FRP.Yampa.returnA -< _16

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _4 <- arr11 (+1) -< _3
    _5 <- pre1 2.540063260732449 -< _3
    _6 <- arr21 (+) -< {_1, _1}
    _7 <- pre1 0.782939052624729 -< _3
    _8 <- pre1 2.4979211944661213 -< _7
    _9 <- pre1 0.15900688111974254 -< _7
    _10 <- arr21 (+) -< {_6, _3}
    _11 <- arr11 (+1) -< _1
    _12 <- arr11 (+1) -< _2
    _13 <- arr21 (*) -< {_12, _11}
    _14 <- arr21 (*) -< {_7, _10}
    _15 <- arr21 (+) -< {_14, _4}
    _16 <- arr21 (*) -< {_8, _11}
    _17 <- pre1 1.3629851664971964 -< _0
    _18 <- pre1 0.20343964196341788 -< _17
    _19 <- arr21 (+) -< {_15, _18}
    _20 <- arr21 (+) -< {_5, _13}
    _1 <- pre1 1.6478318552861126 -< _20
    _2 <- pre1 1.1153464279445746 -< _9
    _3 <- pre1 1.8679849718958177 -< _19

  GenProc.GeneralisedArrow.returnA -< _16|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20