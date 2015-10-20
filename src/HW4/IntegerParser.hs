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
           | TokenVar String
           | TokenNum Integer
           | TokenEnd
    deriving (Show, Eq)

variable :: Parser Token
variable = (\var -> TokenVar var) <$> oneOrMore (satisfy (`elem` ['a'..'z']))

number :: Parser Token
number = (\int -> TokenNum int) <$> posInt

lbracket :: Parser Token
lbracket = const LBracket <$> char '('

rbracket :: Parser Token
rbracket = const RBracket <$> char ')'

operation :: Parser Token
operation = (\op -> TokenOp op) <$> ((const Plus <$> char '+') <|> (const Minus <$> char '-') <|> (const Times <$> char '*') <|> (const Div <$> char '/') <|> (const Pow <$> char '^'))

tokenize :: Parser Token
tokenize = spaces *> (variable <|> number <|> lbracket <|> rbracket <|> operation) <* spaces

data Tree = TermNode Operator Tree Tree | FactorNode Operator Tree Tree | NumNode Integer | UnaryNode Operator Tree | VarNode String deriving (Show, Eq)

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
        _   ->  do
            (tree, toks') <- expression tokens
            if null toks' then return (tree, mempty) else Nothing

expression :: [Token] -> Maybe (Tree, [Token])
expression toks = do
        (termTree, toks') <- term toks
        case lookAhead toks' of
             (TokenOp op) | elem op [Plus, Minus] -> do
                (exTree, toks'') <- expression (accept toks')
                Just (TermNode op termTree exTree, toks'')
             _ -> Just (termTree, toks')

term :: [Token] -> Maybe (Tree, [Token])
term toks = do
   (facTree, toks') <- factor toks
   case lookAhead toks' of
        (TokenOp op) | elem op [Times, Div, Pow] -> do
            (termTree, toks'') <- term (accept toks')
            Just (FactorNode op facTree termTree, toks'')
        _ -> Just (facTree, toks')

factor :: [Token] -> Maybe (Tree, [Token])
factor toks =
    case lookAhead toks of
        (TokenNum x)     -> Just (NumNode x, accept toks)
        (TokenVar str) -> Just (VarNode str, accept toks)
        (TokenOp op) | elem op [Plus, Minus] -> do
            (facTree, toks') <- factor (accept toks)
            Just (UnaryNode op facTree, toks')
        LBracket      -> do
            (expTree, toks') <- expression (accept toks)
            if lookAhead toks' /= RBracket then Nothing else Just (expTree, accept toks')
            --- error "Missing right parenthesis"
        _ -> Nothing --- error $ "Parse error on token: " ++ show toks

testIntegerParser :: IO()
testIntegerParser = do
    let e1 = (Just (FactorNode Times (NumNode 5) (TermNode Plus (NumNode 3) (NumNode 4)), mempty))
    Asserts.equals "IP: Example #1" e1 (runParser parse "5 * (3 + 4)")
    let e2 = Just (TermNode Plus (NumNode 1) (FactorNode Pow (FactorNode Times (NumNode 2) (FactorNode Times (NumNode 3) (TermNode Minus (NumNode 4) (NumNode 5)))) (NumNode 4)),"")
    Asserts.equals "IP: Example #2" e2 (runParser parse "1 + (2 * 3 *( 4 - 5))^4")
    Asserts.equals "IP: Example #3" Nothing (runParser parse "1 * (3 + 4")
    Asserts.equals "IP: Example #4" Nothing (runParser parse "1 * 1 + ")
    Asserts.equals "IP: Example #5" (Just (VarNode "x", mempty)) (runParser parse "x")
    Asserts.equals "IP: Example #6" (Just (TermNode Plus (VarNode "x") (FactorNode Times (VarNode "y") (NumNode 2)),"")) (runParser parse "x + y * 2")
    Asserts.equals "IP: Example #7" Nothing (runParser parse "")