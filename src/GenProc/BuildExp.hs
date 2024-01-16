module GenProc.BuildExp where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Util

-- ***
-- Operations on Exp (), to avoid writing out trees.
-- ***

composeExp :: Exp () -> Exp () -> Exp ()
composeExp = symbolExp ">>>"

parallelExp :: Exp () -> Exp () -> Exp ()
parallelExp = symbolExp "***"

symbolExp :: String -> Exp () -> Exp () -> Exp ()
symbolExp s l r = InfixApp () (paren l) (QVarOp () (qualified (Symbol () s))) (paren r)

arrExp :: Exp () -> Exp ()
arrExp = callExp "arr"

loopExp :: Exp () -> Exp ()
loopExp = callExp "loop"

callExp :: String -> Exp () -> Exp ()
callExp f e = App () (Var () (qualified $ Ident () f)) (paren e)

conExp :: String -> Exp () -> Exp ()
conExp c e = App () (Con () (qualified $ Ident () c)) (paren e)

varExp :: String -> Exp ()
varExp = Var () . qualified . Ident ()

idExp :: Exp ()
idExp = Var () $ qualified $ Ident () "id"

qualified :: Name () -> QName ()
qualified = Qual () (ModuleName () "GenProc.GeneralisedArrow")