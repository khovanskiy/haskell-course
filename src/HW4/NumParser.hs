module HW4.NumParser where

import HW4.Parser
import HW4.AParser
import Data.Functor
import Control.Applicative

--- type Operation = Char
--- data Expression = V Value | O Expression Operation Expression | E Expression deriving Show

data Expression = O Expression Char Expression | V Integer | P Expression Expression deriving Show

--- (stripSpaces $ satisfy (`elem` ['+', '-', '*', '/', '^'])
--parseOperation :: Parser Expression
--parseOperation = (\l o r -> O l o r) <$> parseE <*> char '+' <*> parseE

--parseE :: Parser Expression
--parseE = parseValue <|> parseOperation <|> (char '(' *> (E <$> parseE) <* char ')')

parseE :: Parser Expression
parseE = (\a b -> P a b) <$> parseT <*> parseE'

parseE' :: Parser Expression
parseE' = (\c l r -> O l c r) <$> char '+' <*> parseT <*> parseE'

parseT :: Parser Expression
parseT = (\a b -> P a b) <$> parseF <*> parseT'

parseT' :: Parser Expression
parseT' = (\c l r -> O l c r) <$> char '*' <*> parseF <*> parseT'

parseF :: Parser Expression
parseF = parseN <|> (char '(' *> parseE <* char ')')

parseN :: Parser Expression
parseN = V <$> posInt
