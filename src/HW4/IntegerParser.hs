module HW4.IntegerParser(runParser, tokenize, parse, testIntegerParser) where

import HW4.Parser
import HW4.AParser
import Control.Applicative
import Data.Monoid
import Asserts

data Operator = Plus | Minus | Times | Div | Pow
    deriving (Show, Eq)

data Token = TokenOp Operator
           | LBracket
           | RBracket
           | TokenNum Integer
           | TokenEnd
    deriving (Show, Eq)

number :: Parser Token
number = (\int -> TokenNum int) <$> posInt

lbracket :: Parser Token
lbracket = const LBracket <$> char '('

rbracket :: Parser Token
rbracket = const RBracket <$> char ')'

operation :: Parser Token
operation = (\op -> TokenOp op) <$> ((const Plus <$> char '+') <|> (const Minus <$> char '-') <|> (const Times <$> char '*') <|> (const Div <$> char '/') <|> (const Pow <$> char '^'))

tokenize :: Parser Token
tokenize = spaces *> (number <|> lbracket <|> rbracket <|> operation) <* spaces

data Tree = TermNode Operator Tree Tree | FactorNode Operator Tree Tree | NumNode Integer | UnaryNode Operator Tree deriving (Show, Eq)

lookAhead :: [Token] -> Token
lookAhead [] = TokenEnd
lookAhead (t:_) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (_:ts) = ts

parse :: Parser Tree
parse = Parser $ \toks -> do
    (tokens, _) <- runParser (zeroOrMore tokenize) toks
    case tokens of
        []  ->  Nothing
        _   ->  let (tree, toks') = expression tokens in
                    if null toks' then return (tree, mempty) else Nothing

expression :: [Token] -> (Tree, [Token])
expression toks = do
        let (termTree, toks') = term toks
        case lookAhead toks' of
             (TokenOp op) | elem op [Plus, Minus] ->
                let (exTree, toks'') = expression (accept toks')
                in (TermNode op termTree exTree, toks'')
             _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks =
   let (facTree, toks') = factor toks
   in
      case lookAhead toks' of
         (TokenOp op) | elem op [Times, Div, Pow] ->
            let (termTree, toks'') = term (accept toks')
            in (FactorNode op facTree termTree, toks'')
         _ -> (facTree, toks')

factor :: [Token] -> (Tree, [Token])
factor toks =
    case lookAhead toks of
        (TokenNum x)     -> (NumNode x, accept toks)
        (TokenOp op) | elem op [Plus, Minus] ->
            let (facTree, toks') = factor (accept toks)
            in (UnaryNode op facTree, toks')
        LBracket      ->
            let (expTree, toks') = expression (accept toks)
            in
                if lookAhead toks' /= RBracket
                then error "Missing right parenthesis"
                else (expTree, accept toks')
        _ -> error $ "Parse error on token: " ++ show toks

testIntegerParser :: IO()
testIntegerParser = do
    let e1 = (Just (FactorNode Times (NumNode 5) (TermNode Plus (NumNode 3) (NumNode 4)), mempty))
    Asserts.equals "IP: Example #1" e1 (runParser parse "5 * (3 + 4)")
    let e2 = Just (TermNode Plus (NumNode 1) (FactorNode Pow (FactorNode Times (NumNode 2) (FactorNode Times (NumNode 3) (TermNode Minus (NumNode 4) (NumNode 5)))) (NumNode 4)),"")
    Asserts.equals "IP: Example #2" e2 (runParser parse "1 + (2 * 3 *( 4 - 5))^4")