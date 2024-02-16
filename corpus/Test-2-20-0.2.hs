{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 1.7040553438736175 -< _0
  _2 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _1)
  rec
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_3, _1)
    _6 <- FRP.Yampa.arr (+1) -< _4
    _7 <- FRP.Yampa.arr (uncurry (-)) -< (_6, _5)
    _4 <- iPre 0.199643485627572 -< _0
  _8 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _7)
  _9 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _7)
  _10 <- iPre 3.207795594731105 -< _1
  _11 <- iPre 0.8807004798919013 -< _6
  _12 <- iPre 0.21541055960593772 -< _10
  _13 <- FRP.Yampa.arr (+1) -< _9
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _13)
  _15 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _12)
  _16 <- iPre 1.8414486463159287 -< _11
  _17 <- FRP.Yampa.arr (uncurry (*)) -< (_16, _3)
  _18 <- FRP.Yampa.arr (uncurry (-)) -< (_14, _15)
  _19 <- FRP.Yampa.arr (uncurry (+)) -< (_17, _18)
  _20 <- iPre 4.256521085227646 -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 1.7040553438736175 -< _0
  _2 <- arr21 (*) -< {_1, _0}
  _3 <- arr21 (*) -< {_2, _1}
  rec
    _5 <- arr21 (+) -< {_3, _1}
    _6 <- arr11 (+1) -< _4
    _7 <- arr21 (-) -< {_6, _5}
    _4 <- pre1 0.199643485627572 -< _0

  _8 <- arr21 (+) -< {_6, _7}
  _9 <- arr21 (*) -< {_8, _7}
  _10 <- pre1 3.207795594731105 -< _1
  _11 <- pre1 0.8807004798919013 -< _6
  _12 <- pre1 0.21541055960593772 -< _10
  _13 <- arr11 (+1) -< _9
  _14 <- arr21 (*) -< {_12, _13}
  _15 <- arr21 (-) -< {_12, _12}
  _16 <- pre1 1.8414486463159287 -< _11
  _17 <- arr21 (*) -< {_16, _3}
  _18 <- arr21 (-) -< {_14, _15}
  _19 <- arr21 (+) -< {_17, _18}
  _20 <- pre1 4.256521085227646 -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4