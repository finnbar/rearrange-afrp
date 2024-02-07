{-# LANGUAGE Arrows #-}
module Test0 where

import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

yampa :: SF Double Double
yampa = proc _0 -> do
  rec
    _7 <- FRP.Yampa.arr (uncurry (+)) -< (_5, _0)
    _8 <- FRP.Yampa.arr (uncurry (+)) -< (_7, _5)
    _9 <- iPre 3.940804268503294e-2 -< _5
    _10 <- iPre 2.3046722784924296 -< _7
    _11 <- FRP.Yampa.arr (uncurry (*)) -< (_10, _5)
    _12 <- iPre 1.0652705102908795 -< _10
    _13 <- iPre 2.9778680377630664 -< _0
    _14 <- FRP.Yampa.arr (+1) -< _5
    _15 <- FRP.Yampa.arr (+1) -< _7
    _16 <- FRP.Yampa.arr (+1) -< _5
    _17 <- FRP.Yampa.arr (uncurry (*)) -< (_3, _1)
    _18 <- FRP.Yampa.arr (uncurry (-)) -< (_12, _2)
    _19 <- iPre 0.8055052223518826 -< _16
    _20 <- FRP.Yampa.arr (+1) -< _9
    _21 <- FRP.Yampa.arr (uncurry (*)) -< (_8, _11)
    _22 <- FRP.Yampa.arr (+1) -< _20
    _23 <- iPre 1.430460849164872 -< _6
    _24 <- iPre 1.0528795558970596 -< _17
    _25 <- FRP.Yampa.arr (+1) -< _13
    _26 <- FRP.Yampa.arr (uncurry (-)) -< (_22, _24)
    _27 <- FRP.Yampa.arr (+1) -< _4
    _28 <- FRP.Yampa.arr (uncurry (*)) -< (_26, _21)
    _29 <- FRP.Yampa.arr (uncurry (-)) -< (_18, _25)
    _30 <- iPre 2.0980871369484664 -< _27
    _1 <- iPre 0.6926683239392014 -< _14
    _2 <- iPre 0.6806848938402984 -< _19
    _3 <- iPre 1.7950032922253862 -< _30
    _4 <- iPre 1.3886506480751761 -< _15
    _5 <- iPre 1.3205881251152056 -< _23
    _6 <- iPre 1.3755281539544248 -< _29
  FRP.Yampa.returnA -< _28

afrp :: AFRP _ (V Double) (V Double)
afrp = [gap|proc _0 -> do
  rec
    _7 <- arr21 (+) -< {_5, _0}
    _8 <- arr21 (+) -< {_7, _5}
    _9 <- pre1 3.940804268503294e-2 -< _5
    _10 <- pre1 2.3046722784924296 -< _7
    _11 <- arr21 (*) -< {_10, _5}
    _12 <- pre1 1.0652705102908795 -< _10
    _13 <- pre1 2.9778680377630664 -< _0
    _14 <- arr11 (+1) -< _5
    _15 <- arr11 (+1) -< _7
    _16 <- arr11 (+1) -< _5
    _17 <- arr21 (*) -< {_3, _1}
    _18 <- arr21 (-) -< {_12, _2}
    _19 <- pre1 0.8055052223518826 -< _16
    _20 <- arr11 (+1) -< _9
    _21 <- arr21 (*) -< {_8, _11}
    _22 <- arr11 (+1) -< _20
    _23 <- pre1 1.430460849164872 -< _6
    _24 <- pre1 1.0528795558970596 -< _17
    _25 <- arr11 (+1) -< _13
    _26 <- arr21 (-) -< {_22, _24}
    _27 <- arr11 (+1) -< _4
    _28 <- arr21 (*) -< {_26, _21}
    _29 <- arr21 (-) -< {_18, _25}
    _30 <- pre1 2.0980871369484664 -< _27
    _1 <- pre1 0.6926683239392014 -< _14
    _2 <- pre1 0.6806848938402984 -< _19
    _3 <- pre1 1.7950032922253862 -< _30
    _4 <- pre1 1.3886506480751761 -< _15
    _5 <- pre1 1.3205881251152056 -< _23
    _6 <- pre1 1.3755281539544248 -< _29

  GenProc.GeneralisedArrow.returnA -< _28|]

codeLen :: Int
codeLen = 30
codeRecLen :: Int
codeRecLen = 30