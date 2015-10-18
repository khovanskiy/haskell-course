module HW4.AParser(Parser(..), zeroOrMore, oneOrMore, spaces, ident, stripSpaces) where

import HW4.Parser
import Data.Functor
import Data.Char
import Control.Applicative

--- #1
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

--- #2
spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (++) <$> (oneOrMore $ satisfy isAlpha) <*> (zeroOrMore $ satisfy isAlphaNum)

--- #3
type Ident = String
data Atom = N Integer | I Ident deriving Show
data SExpr = A Atom | Comb [SExpr] deriving Show

stripSpaces :: Parser a -> Parser a
stripSpaces p = spaces *> p <* spaces

parseAtom :: Parser Atom
parseAtom = (N <$> stripSpaces posInt) <|> (I <$> stripSpaces ident)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> ((A <$> parseAtom) <|> (char '(' *> (Comb <$> zeroOrMore (stripSpaces parseSExpr)) <* char ')')) <* spaces


