{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _8 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _7)
    _9 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _4)
    _10 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _2)
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_7, _4)
    _12 <- FRP.Yampa.arr (uncurry (-)) -< (_3, _8)
    _13 <- FRP.Yampa.arr (+1) -< _1
    _14 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _1)
    _15 <- FRP.Yampa.arr (+1) -< _5
    _16 <- iPre 2.353923486113482 -< _12
    _17 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _10)
    _18 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _4)
    _19 <- FRP.Yampa.arr (uncurry (+)) -< (_13, _17)
    _20 <- FRP.Yampa.arr (uncurry (+)) -< (_14, _13)
    _1 <- iPre 2.5757291827666555 -< _16
    _2 <- iPre 1.6246574138027496 -< _11
    _3 <- iPre 2.7899563701225683 -< _18
    _4 <- iPre 2.8275772810587894 -< _19
    _5 <- iPre 2.9741061175970867 -< _9
    _6 <- iPre 1.6324409813283286e-2 -< _15
    _7 <- iPre 2.420324576295194 -< _0
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _8 <- arr21 (*) -< {_6, _7}
    _9 <- arr21 (+) -< {_7, _4}
    _10 <- arr21 (+) -< {_5, _2}
    _11 <- arr21 (*) -< {_7, _4}
    _12 <- arr21 (-) -< {_3, _8}
    _13 <- arr11 (+1) -< _1
    _14 <- arr21 (*) -< {_12, _1}
    _15 <- arr11 (+1) -< _5
    _16 <- pre1 2.353923486113482 -< _12
    _17 <- arr21 (+) -< {_7, _10}
    _18 <- arr21 (*) -< {_6, _4}
    _19 <- arr21 (+) -< {_13, _17}
    _20 <- arr21 (+) -< {_14, _13}
    _1 <- pre1 2.5757291827666555 -< _16
    _2 <- pre1 1.6246574138027496 -< _11
    _3 <- pre1 2.7899563701225683 -< _18
    _4 <- pre1 2.8275772810587894 -< _19
    _5 <- pre1 2.9741061175970867 -< _9
    _6 <- pre1 1.6324409813283286e-2 -< _15
    _7 <- pre1 2.420324576295194 -< _0

  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 20