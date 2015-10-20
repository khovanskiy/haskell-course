module HW5.Evaluater(evaluateTree, evaluate, testEvaluater) where

import Control.Monad.Reader
import HW4.ExpressionParser
import HW2.Map
import HW2.ListMap
import Asserts

evalBinary :: (Map m, Floating a) => Tree a -> (a -> a -> a) -> Tree a -> Reader (m String a) (Maybe a)
evalBinary lhs op rhs = asks $ \env -> do
    x <- runReader (evalTree lhs) env
    y <- runReader (evalTree rhs) env
    return (op x y)

evalUnary :: (Map m, Floating a) => (a -> a) -> Tree a -> Reader (m String a) (Maybe a)
evalUnary op rhs = asks $ \env -> do
    x <- runReader (evalTree rhs) env
    return (op x)

evalTree :: (Map m, Floating a) => Tree a -> Reader (m String a) (Maybe a)
evalTree (NumNode num) = return (Just num)
evalTree (FactorNode op lhs rhs) = case op of
    Times   -> evalBinary lhs (*) rhs
    Div     -> evalBinary lhs (/) rhs
    Pow     -> evalBinary lhs (**) rhs
evalTree (TermNode op lhs rhs) = case op of
    Plus    -> evalBinary lhs (+) rhs
    Minus   -> evalBinary lhs (-) rhs
evalTree (UnaryNode op tree) = case op of
    Plus    -> evalUnary (0+) tree
    Minus   -> evalUnary (0-) tree
evalTree (VarNode var) = asks $ \env -> get env var
evalTree (AssignNode key valTree child) = asks $ \env -> do
    newVal <- runReader (evalTree valTree) env
    runReader (evalTree child) (put env key newVal)

---evaluate :: (Map m) => m Integer Integer -> String
---evaluate = runReader $ (return ("") :: Reader (m Integer Integer) String)
---evaluate :: (Map m, Floating a) => Tree a -> m String a -> Maybe a
evaluateTree :: (Map m, Floating a) => Tree a -> m String a -> Maybe a
evaluateTree tree = runReader (evalTree tree)

evaluate :: Map m => String -> m String Double -> Maybe Double
evaluate x = \m -> case runParser parser x of
    Nothing -> Nothing
    (Just (tree, chars)) -> if null chars then evaluateTree tree m else Nothing

testEvaluater :: IO()
testEvaluater = do
    let m = fromList [("x", 3), ("y", 2), ("z", 5)]
    Asserts.equals "E: Example #1" Nothing (evaluate "" m)
    Asserts.equals "E: Example #2" Nothing (evaluate "3+" m)
    Asserts.equals "E: Example #3" (Just (-3)) (evaluate "-3" m)
    Asserts.equals "E: Example #4" (Just (-5)) (evaluate "-3*x + y^2" m)
    Asserts.equals "E: Example #5" Nothing (evaluate "x + y + z + w" m)
    let e6 = (TermNode Plus (AssignNode "w" (NumNode 5) (VarNode "w")) (AssignNode "w" (NumNode 4) (VarNode "w")))
    Asserts.equals "E: Example #6" (Just 9) (evaluateTree e6 m)
