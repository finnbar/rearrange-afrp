module GenProc.ProcTH where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH as TH

import Language.Haskell.Exts (Language(Haskell2010), KnownExtension (Arrows), Extension (EnableExtension))
import Language.Haskell.Meta.Syntax.Translate
import GenProc.TransformProc (transformProc)
import GenProc.PairPattern (transformCurlySyntax)
import GenProc.Optimise (optimise)

gap :: TH.QuasiQuoter
gap = TH.QuasiQuoter { TH.quoteExp = qexp }

qexp :: String -> TH.Q TH.Exp
qexp inp = let
    withCurly = transformCurlySyntax inp
    parseMode = defaultParseMode {baseLanguage = Haskell2010, extensions = [EnableExtension Arrows]}
    -- TODO: Do I need to enable more extensions depending on the LANGUAGE pragmas present in the file?
    presult :: Exp SrcSpanInfo
    presult = case parseWithMode parseMode withCurly of
        ParseOk a -> a
        ParseFailed loc str -> error $ show (loc, str)
    in procTransform $ fmap (const ()) presult

procTransform :: Exp () -> TH.Q TH.Exp
procTransform (Proc _ pat exp) = let
    inner = return . toExp $ transformProc pat exp
    in [| optimise $(inner) |]
procTransform _ = error "The gap QuasiQuoter should only be used on proc statements."