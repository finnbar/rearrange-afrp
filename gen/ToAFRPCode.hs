module ToAFRPCode where

import GenerateProcCode

import qualified Text.RawString.QQ as Q (r)
import Hedgehog.Gen (sample)

-- This generates some code, and then makes a file with the given filename containing that code.
generateFile :: Int -> Int -> FilePath -> IO ()
generateFile codeLen codeRecLen modNam = do
    procCode <- sample $ generateProcCode codeLen codeRecLen
    let yampaCode = toYampa procCode
        afrpCode = toAFRP procCode
        moduleName = "{-# LANGUAGE Arrows #-}\nmodule " ++ modNam ++ " where\n\n"
        yampaDef = "yampa :: SF Int Int\nyampa = " ++ yampaCode ++ "\n\n"
        afrpDef = "afrp :: AFRP _ (V Int) (V Int)\nafrp = " ++ afrpCode
    writeFile ("generated/" ++ modNam ++ ".hs") $ moduleName ++ header ++ yampaDef ++ afrpDef

singleVarPrintout :: SingleVar -> String
singleVarPrintout v = "_" ++ show v

pairVarPrintoutYampa :: PairVar -> String
pairVarPrintoutYampa (l, r) = "(_" ++ show l ++ ", _" ++ show r ++ ")"

pairVarPrintoutAFRP :: PairVar -> String
pairVarPrintoutAFRP (l, r) = "{_" ++ show l ++ ", _" ++ show r ++ "}"

-- Generate a string of Yampa code from a ProcCode object.
toYampa :: ProcCode -> String
toYampa (PC lines output) = "proc _0 -> do\n" ++ linesToYampa lines "  " ++ "  FRP.Yampa.returnA -< " ++ singleVarPrintout output

linesToYampa :: [Line] -> String -> String
linesToYampa [] _ = ""
linesToYampa (l : ls) indent = case l of
    Pre val inp out -> printIt (singleVarPrintout inp) ("iPre " ++ show val) (singleVarPrintout out) ++ linesToYampa ls indent
    Add inps out -> printIt (pairVarPrintoutYampa inps) "FRP.Yampa.arr (uncurry (+))" (singleVarPrintout out) ++ linesToYampa ls indent
    Sub inps out -> printIt (pairVarPrintoutYampa inps) "FRP.Yampa.arr (uncurry (-))" (singleVarPrintout out) ++ linesToYampa ls indent
    Mul inps out -> printIt (pairVarPrintoutYampa inps) "FRP.Yampa.arr (uncurry (*))" (singleVarPrintout out) ++ linesToYampa ls indent
    Inc inp out -> printIt (singleVarPrintout inp) "FRP.Yampa.arr (+1)" (singleVarPrintout out) ++ linesToYampa ls indent
    RecLine lines -> indent ++ "rec\n" ++ linesToYampa lines ("  " ++ indent) ++ linesToYampa ls indent
    where
        -- Turns inp fname output into output <- fname -< inp
        printIt :: String -> String -> String -> String
        printIt inp fname output = indent ++ output ++ " <- " ++ fname ++ " -< " ++ inp ++ "\n"

toAFRP :: ProcCode -> String
toAFRP (PC lines output) = "[gap|proc _0 -> do\n" ++ linesToAFRP lines "  " ++ "  GenProc.GeneralisedArrow.returnA -< " ++ singleVarPrintout output ++ "|]"

linesToAFRP :: [Line] -> String -> String
linesToAFRP [] _ = ""
linesToAFRP (l : ls) indent = case l of
    Pre val inp out -> printIt (singleVarPrintout inp) ("pre1 " ++ show val) (singleVarPrintout out) ++ linesToAFRP ls indent
    Add inps out -> printIt (pairVarPrintoutAFRP inps) "arr21 (+)" (singleVarPrintout out) ++ linesToAFRP ls indent
    Sub inps out -> printIt (pairVarPrintoutAFRP inps) "arr21 (-)" (singleVarPrintout out) ++ linesToAFRP ls indent
    Mul inps out -> printIt (pairVarPrintoutAFRP inps) "arr21 (*)" (singleVarPrintout out) ++ linesToAFRP ls indent
    Inc inp out -> printIt (singleVarPrintout inp) "arr11 (+1)" (singleVarPrintout out) ++ linesToAFRP ls indent
    RecLine lines -> indent ++ "rec\n" ++ linesToAFRP lines ("  " ++ indent) ++ "\n" ++ linesToAFRP ls indent
    where
        -- Turns inp fname output into output <- fname -< inp
        printIt :: String -> String -> String -> String
        printIt inp fname output = indent ++ output ++ " <- " ++ fname ++ " -< " ++ inp ++ "\n"

header :: String
header = [Q.r|import FRP.Yampa
import RAFRP
import GenProc.GeneralisedArrow
import GenProc.ProcTH

|]