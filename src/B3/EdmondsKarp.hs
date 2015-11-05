module B3.EdmondsKarp(readGraph, writeGraph, maxFlow) where

import qualified Data.Map.Lazy as Map
import Prelude hiding (foldl, foldr, mapM_)
import Data.Functor
import B3.Graph
import System.Environment
import System.IO
import Data.Monoid
import Data.Foldable
import Control.Monad

type FlowGraph = Graph String (Int, Int)

readEdge :: String -> Edge String (Int, Int)
readEdge s = Edge a b (0, c) where
    slices = words s
    a = slices !! 0
    b = slices !! 1
    c = read (slices !! 2) :: Int

readGraph :: FilePath -> IO FlowGraph
readGraph filename = do
    content <- readFile filename
    ---(\a b c -> Edge a b (0, c)) <$>
    ---mapM_ (\s -> print $ readEdge s) $ lines content
    let graph = foldl folder (Graph empty) (readEdge <$> lines content) where
        folder g e = putEdge g2 e where
            g2 = initVertex g (from e)
    return graph

writeEdge :: Edge String (Int, Int) -> String
writeEdge (Edge a b (c, d)) = a ++ " " ++ b ++ " " ++ show c ++ "/" ++ show d

writeVertex :: Vertex String (Int, Int) -> [String]
writeVertex v = foldl (\s e -> (writeEdge e):s) mempty (successors v)

writeGraph :: FlowGraph -> FilePath -> IO ()
writeGraph graph filename = do
    handle <- openFile filename WriteMode
    --mapM_ (hPutStrLn handle . join . writeVertex) (vertexes graph)
    let content = unlines $ join (writeVertex <$> (Map.elems $ vertexes graph))
    hPutStrLn handle content
    hClose handle

maxFlow :: FlowGraph -> String -> String -> Int
maxFlow graph source target = 0
