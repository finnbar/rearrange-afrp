{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  _1 <- iPre 0.222745604278971 -< _0
  _2 <- iPre 1.9909246958411733 -< _1
  _3 <- FRP.Yampa.arr (uncurry (*)) -< (_0, _1)
  _4 <- FRP.Yampa.arr (+1) -< _0
  _5 <- iPre 0.43193054770467004 -< _4
  _6 <- iPre 5.0898817052579454e-2 -< _3
  _7 <- FRP.Yampa.arr (+1) -< _3
  _8 <- FRP.Yampa.arr (uncurry (-)) -< (_2, _5)
  _9 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _8)
  _10 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _3)
  _11 <- FRP.Yampa.arr (uncurry (+)) -< (_6, _7)
  _12 <- iPre 2.9211920017564967 -< _11
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _10)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_13, _12)
  _15 <- iPre 2.007651300422881 -< _9
  _16 <- FRP.Yampa.arr (+1) -< _15
  _17 <- iPre 2.6331839331823645 -< _16
  _18 <- iPre 8.577407712337341e-2 -< _17
  _19 <- iPre 1.2395147258577686 -< _18
  _20 <- FRP.Yampa.arr (uncurry (*)) -< (_14, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  _1 <- pre1 0.222745604278971 -< _0
  _2 <- pre1 1.9909246958411733 -< _1
  _3 <- arr21 (*) -< {_0, _1}
  _4 <- arr11 (+1) -< _0
  _5 <- pre1 0.43193054770467004 -< _4
  _6 <- pre1 5.0898817052579454e-2 -< _3
  _7 <- arr11 (+1) -< _3
  _8 <- arr21 (-) -< {_2, _5}
  _9 <- arr21 (+) -< {_5, _8}
  _10 <- arr21 (*) -< {_3, _3}
  _11 <- arr21 (+) -< {_6, _7}
  _12 <- pre1 2.9211920017564967 -< _11
  _13 <- arr21 (-) -< {_4, _10}
  _14 <- arr21 (*) -< {_13, _12}
  _15 <- pre1 2.007651300422881 -< _9
  _16 <- arr11 (+1) -< _15
  _17 <- pre1 2.6331839331823645 -< _16
  _18 <- pre1 8.577407712337341e-2 -< _17
  _19 <- pre1 1.2395147258577686 -< _18
  _20 <- arr21 (*) -< {_14, _19}
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 0