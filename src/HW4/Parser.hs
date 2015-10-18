module HW4.Parser(Parser(..), satisfy, char, posInt, abParser, abParser_, intPair, intOrUppercase) where

import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f where
    f [] = Nothing
    f (x:xs)
        | p x = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f where
    f xs
        | null ns = Nothing
        | otherwise = Just (read ns, rest) where
            (ns, rest) = span isDigit xs

--- #1
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c)   = (f a, c)

instance Functor Parser where
    fmap f p = Parser $ fmap (first f) . (runParser p)

--- #2
instance Applicative Parser where
    pure a = Parser $ (\x -> Just (a, x))
    Parser p1 <*> Parser p2 = Parser $ \input -> do
        (f, r1) <- p1 input
        (x, r2) <- p2 r1
        return (f x, r2)

--- #3
abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _  -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

--- #4
instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser f <|> Parser g = Parser $ \x -> f x <|> g x

--- #5
intOrUppercase :: Parser ()
intOrUppercase = (const ()) <$> posInt <|> (const ()) <$> satisfy (isUpper)