{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _3 <- iPre 0.985169515424507 -< _2
    _4 <- FRP.Yampa.arr (+1) -< _0
    _5 <- FRP.Yampa.arr (uncurry (+)) -< (_4, _2)
    _6 <- iPre 2.730014605666796 -< _3
    _7 <- iPre 1.1872105567708013 -< _6
    _8 <- iPre 2.8487450362124966 -< _1
    _9 <- FRP.Yampa.arr (uncurry (*)) -< (_6, _7)
    _10 <- FRP.Yampa.arr (uncurry (*)) -< (_2, _4)
    _1 <- iPre 2.9574032054959165 -< _2
    _2 <- iPre 0.5331315518270935 -< _10
  _11 <- FRP.Yampa.arr (+1) -< _8
  _12 <- FRP.Yampa.arr (uncurry (+)) -< (_1, _11)
  _13 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _1)
  _14 <- FRP.Yampa.arr (uncurry (*)) -< (_12, _9)
  _15 <- iPre 1.2748257428425613 -< _13
  _16 <- FRP.Yampa.arr (+1) -< _14
  _17 <- FRP.Yampa.arr (+1) -< _16
  _18 <- FRP.Yampa.arr (uncurry (*)) -< (_17, _15)
  _19 <- iPre 2.1446025400879325 -< _18
  _20 <- FRP.Yampa.arr (+1) -< _19
  FRP.Yampa.returnA -< _20

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _3 <- pre1 0.985169515424507 -< _2
    _4 <- arr11 (+1) -< _0
    _5 <- arr21 (+) -< {_4, _2}
    _6 <- pre1 2.730014605666796 -< _3
    _7 <- pre1 1.1872105567708013 -< _6
    _8 <- pre1 2.8487450362124966 -< _1
    _9 <- arr21 (*) -< {_6, _7}
    _10 <- arr21 (*) -< {_2, _4}
    _1 <- pre1 2.9574032054959165 -< _2
    _2 <- pre1 0.5331315518270935 -< _10

  _11 <- arr11 (+1) -< _8
  _12 <- arr21 (+) -< {_1, _11}
  _13 <- arr21 (+) -< {_5, _1}
  _14 <- arr21 (*) -< {_12, _9}
  _15 <- pre1 1.2748257428425613 -< _13
  _16 <- arr11 (+1) -< _14
  _17 <- arr11 (+1) -< _16
  _18 <- arr21 (*) -< {_17, _15}
  _19 <- pre1 2.1446025400879325 -< _18
  _20 <- arr11 (+1) -< _19
  GenProc.GeneralisedArrow.returnA -< _20|]

codeLen :: Int
codeLen = 20
codeRecLen :: Int
codeRecLen = 10