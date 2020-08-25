-- | Module that provides a parser for λ-terms.
module Parser (termParser) where

import Control.Applicative ((<|>), pure)
import Core (Sym (Sym), Term (Var, Abs, App))
import Data.Attoparsec.Text
    ( Parser
    , many1
    , skip
    , skipMany
    , space
    , takeWhile1
    )
import Data.Bool (Bool, (||), not)
import Data.Char (Char, isSpace)
import Data.Eq ((==))
import Data.Foldable (foldl1)
import Data.Function (($))

-- | Parser for a term.
--
--   A term may be composed of applications, abstractions or
--   variables. We combine their respective parsers. Note that
--   the order of the parsers is significant. If the first one
--   succeeds, it will be used, even if there is trailing text.
termParser :: Parser Term
termParser = appParser' <|> absParser' <|> varParser'

-- | Variable parser that also checks for parenthesis.
varParser' :: Parser Term
varParser' = varParser <|> parens varParser

-- | Application parser that also checks for parenthesis.
appParser' :: Parser Term
appParser' = appParser <|> parens appParser

-- | Abstraction parser that also checks for parenthesis.
absParser' :: Parser Term
absParser' = absParser <|> parens absParser

-- | Application parser.
--
--   This parser creates a application value by repeatedly
--   parsing terms. If only a single variable is found, then
--   the variable is returned as is.
--
--   Note that while this parser generally checks for forms
--   that are parenthesized as well as not, it *only* checks for
--   parenthesized forms in the case of recursive application.
--   Otherwise, we would descend into an infinite loop. This
--   works because application is left-associative.
appParser :: Parser Term
appParser = do
    l <- many1 (parens appParser <|> absParser' <|> varParser')
    pure $ foldl1 (\p q -> App p q) l

-- | Abstraction parser.
--
--   Abstraction consumes everything after the dot until the
--   end of the term. This parser supports multi-variable
--   abstractions such as \x y.y w (equivalent to \x.\y.y w).
absParser :: Parser Term
absParser = do
    chompTrailingSpace (skip isAbs)
    l <- many1 symParser
    chompTrailingSpace (skip isDot)
    q <- termParser
    pure $ lam q l
  where
    lam q (x:[]) = Abs x q
    lam q (x:xs) = Abs x (lam q xs)
    lam q [] = Abs (Sym "?") q -- Inconceivable!

-- | Variable parser.
varParser :: Parser Term
varParser = do
    sym <- symParser
    pure $ Var sym

-- | Symbol parser.
--
--   Any character not explicitly disallowed is allowed.
symParser :: Parser Sym
symParser = do
    txt <- chompTrailingSpace (takeWhile1 isSymChar)
    pure $ Sym txt

-- | Remove trailing whitespace.
--
--   For whitespace-insensitive parsing such as this, the
--   simplest strategy for consistency is for every parser
--   to expect to start from a significant character, and
--   always internally consume whitespace at the end (so
--   the next parser can expect to start from a signficant
--   character again). All parsers must use this combinator
--   to skip whitespace at the end.
chompTrailingSpace :: Parser a -> Parser a
chompTrailingSpace parser = do
    res <- parser
    skipMany space
    pure $ res

-- | Parenthesize.
--
--   This combinator adds parsing for parenthesis around
--   any existing parser. In general, we want to check for
--   both non-parenthesized and parenthesized versions of
--   terms, but in some cases we specifically need one or
--   the other.
parens :: Parser a -> Parser a
parens parser = do
    chompTrailingSpace $ skip isLParen
    res <- parser
    chompTrailingSpace $ skip isRParen
    pure $ res

-- | Is the character one we can include in a symbol?
isSymChar :: Char -> Bool
isSymChar c = not (isAbs c || isDot c || isParen c || isSpace c)

-- | Is the character a lambda or backslash?
isAbs :: Char -> Bool
isAbs c = ('\\' == c) || ('λ' == c)

-- | Is the character a dot?
isDot :: Char -> Bool
isDot = ('.' ==)

-- | Is the character a parenthesis?
isParen :: Char -> Bool
isParen c = isLParen c || isRParen c

-- | Is the character a left parenthesis?
isLParen :: Char -> Bool
isLParen = ('(' ==)

-- | Is the character a right parenthesis?
isRParen :: Char -> Bool
isRParen = (')' ==)
