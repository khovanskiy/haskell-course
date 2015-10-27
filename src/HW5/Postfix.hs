module HW5.Postfix(evalPostfix, testPostfix) where

import HW4.AParser
import HW4.ExpressionParser
import Control.Monad.State
import Data.Functor
import Asserts

type Stack = [Double]

pop :: State Stack Double
pop = state $ \(x:xs) -> (x, xs)

push :: Double -> State Stack ()
push a = state $ \xs -> ((), a:xs)

comp :: [Token] -> Double
comp xs = head $ execState (foldr1 (>>) (step <$> xs)) []

step :: Token -> State Stack ()
step t = case t of
    TokenNum num    -> push num
    TokenOp op      -> do
        x <- pop
        y <- pop
        case op of
            Plus    -> push (y + x)
            Minus   -> push (y - x)
            Times   -> push (y * x)
            Div     -> push (y / x)
            Pow     -> push (y ** x)

evalPostfix :: String -> Maybe Double
evalPostfix s = do
    case runParser (zeroOrMore tokenize) s of
        Just (tokens, _) -> Just (comp tokens)
        _ -> Nothing

testPostfix :: IO()
testPostfix = do
    Asserts.equals "PF: Example #1" (Just 7) (evalPostfix "3 4 +")
    Asserts.equals "PF: Example #2" (Just 15) (evalPostfix "1 2 + 4 * 3 +")
    Asserts.equals "PF: Example #3"(Just 3.5) (evalPostfix "3 4 2 * 1 5 - 2 ^ / +")

