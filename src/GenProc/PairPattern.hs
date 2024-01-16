-- Parses patterns/expressions of the form {x, y}, transforming them into Pair (One x) (One y).
-- Also allows nesting - {x, {y, z}} translates into Pair (One x) (Pair (One y) (One z)).
-- This saves programmers a lot of pattern matching work.
-- This is run _before_ proc notation transformation, and is run outside of the translation
-- since haskell-src-exts doesn't know what {} are.
-- We need to only take commas in the top scope, so skip commas within () and [].

module GenProc.PairPattern (transformCurlySyntax) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data ParsedPair = OneParsed String | PairParsed ParsedPair ParsedPair
type Parser = Parsec Void String

transformCurlySyntax :: String -> String
transformCurlySyntax s = case parse parsePairs "" s of
    Left err -> error $ show err
    Right res -> res

parsePairs :: Parser String
parsePairs = do
    skipped <- many (anySingleBut '{')
    (eof >> return skipped) <|> (pair >>= continue skipped)
    where
        continue skip pp = do
            rest <- parsePairs
            return $ skip ++ "(" ++ toConstructors pp ++ ")" ++ rest

-- NOTE: We deliberately do not add One here.
-- This simplifies the rest of our parsing - we can add One to every input/output without
-- worrying about which case is which.
toConstructors :: ParsedPair -> String
toConstructors (OneParsed s) = s
toConstructors (PairParsed l r) = "GenProc.GeneralisedArrow.Pair (" ++ toConstructors l ++ ") (" ++ toConstructors r ++ ")"

-- Parses {x, y}, where x and y are variables, a tuple, a list or {,} itself.
pair :: Parser ParsedPair
pair = do
    _ <- single '{'
    hspace
    x <- term
    hspace
    _ <- single ','
    hspace
    y <- term
    hspace
    _ <- single '}'
    return $ PairParsed x y

term :: Parser ParsedPair
term = pair <|> (OneParsed <$> tuple) <|> (OneParsed <$> list) <|> (OneParsed <$> identifier)
    where
        tuple = between' '(' ')'
        list = between' '[' ']'
        identifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '\'')

between' :: Char -> Char -> Parser String
between' l r = do
    mid <- between (single l) (single r) (many $ anySingleBut r)
    return $ l : mid ++ [r]
