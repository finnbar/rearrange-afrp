{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import AFRP
import GenProc.ProcTH

yampa :: FRP.Yampa.SF Double Double
yampa = proc _0 -> do
  rec
    _2 <- FRP.Yampa.arr (+1) -< _1
    _3 <- FRP.Yampa.arr (uncurry (*)) -< (_1, _0)
    _4 <- iPre 1.8763666959812566 -< _3
    _1 <- iPre 0.966271170776743 -< _2
  _5 <- iPre 0.33427960611596275 -< _4
  _6 <- FRP.Yampa.arr (+1) -< _2
  _7 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _6)
  _8 <- iPre 0.8726826506399853 -< _5
  _9 <- FRP.Yampa.arr (+1) -< _3
  _10 <- iPre 2.203375075565781 -< _6
  _11 <- FRP.Yampa.arr (uncurry (*)) -< (_4, _3)
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_10, _7)
  _13 <- FRP.Yampa.arr (uncurry (-)) -< (_4, _2)
  _14 <- iPre 1.4918533933039824 -< _12
  _15 <- FRP.Yampa.arr (uncurry (+)) -< (_8, _13)
  _16 <- iPre 2.3442203932998487 -< _11
  _17 <- FRP.Yampa.arr (uncurry (+)) -< (_16, _15)
  _18 <- iPre 1.7642034540039302 -< _14
  _19 <- FRP.Yampa.arr (uncurry (*)) -< (_18, _17)
  _20 <- FRP.Yampa.arr (uncurry (+)) -< (_9, _19)
  FRP.Yampa.returnA -< _20

afrp :: AFRP.SF _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _2 <- arr11 (+1) -< _1
    _3 <- arr21 (*) -< {_1, _0}
    _4 <- pre1 1.8763666959812566 -< _3
    _1 <- pre1 0.966271170776743 -< _2

  _5 <- pre1 0.33427960611596275 -< _4
  _6 <- arr11 (+1) -< _2
  _7 <- arr21 (*) -< {_6, _6}
  _8 <- pre1 0.8726826506399853 -< _5
  _9 <- arr11 (+1) -< _3
  _10 <- pre1 2.203375075565781 -< _6
  _11 <- arr21 (*) -< {_4, _3}
  _12 <- arr21 (+) -< {_10, _7}
  _13 <- arr21 (-) -< {_4, _2}
  _14 <- pre1 1.4918533933039824 -< _12
  _15 <- arr21 (+) -< {_8, _13}
  _16 <- pre1 2.3442203932998487 -< _11
  _17 <- arr21 (+) -< {_16, _15}
  _18 <- pre1 1.7642034540039302 -< _14
  _19 <- arr21 (*) -< {_18, _17}
  _20 <- arr21 (+) -< {_9, _19}
  AFRP.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 4