module B3.EdmondsKarp(DWork(..), readGraph, writeGraph, dijkstra, maxFlow) where

import qualified Data.Map.Lazy as Map
import Data.Functor
import B3.Graph
import System.IO
import Data.Monoid
import Data.Foldable(for_, toList, foldl)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
---import Control.Monad.Loops
import qualified Data.PQueue.Min as PQ

type FlowGraph = Graph String (Int, Int)
type FlowEdge = Edge String (Int, Int)
type FlowVertex = Vertex String (Int, Int)

data DWork = DWork {weights :: Map.Map String Int, expanded :: Map.Map String Bool, queue :: PQ.MinQueue DState, cameFrom :: Map.Map String String}
    deriving (Show)

data DState = DState FlowVertex Int
    deriving (Show)

instance Eq (DState) where
    (DState _ p1) == (DState _ p2) = p1 == p2

instance Ord (DState) where
    (DState _ p1) <= (DState _ p2) = p1 <= p2

readEdge :: String -> FlowEdge
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
    let graph = Data.Foldable.foldl folder (Graph empty) (readEdge <$> lines content) where
        folder g e = putEdge g3 e where
            g2 = initVertex g (from e)
            g3 = initVertex g2 (to e)
    return graph

writeEdge :: FlowEdge -> String
writeEdge (Edge a b (c, d)) = a ++ " " ++ b ++ " " ++ show c ++ "/" ++ show d

writeVertex :: FlowVertex -> [String]
writeVertex v = Map.foldl (\s e -> (writeEdge e):s) mempty (successors v)

writeGraph :: FlowGraph -> FilePath -> IO ()
writeGraph graph filename = do
    handle <- openFile filename WriteMode
    let content = unlines $ join (writeVertex <$> (Map.elems $ vertexes graph))
    hPutStrLn handle content
    hClose handle

debug :: (MonadIO m) => String -> m ()
debug x = (liftIO . putStrLn) x

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()

isEmpty :: StateT DWork IO Bool
isEmpty = do
    work <- get
    return $ PQ.null (queue work)

dijkstra :: FlowGraph -> FlowVertex -> FlowVertex -> StateT DWork IO [FlowEdge]
dijkstra graph s t = do
    let list = Data.Foldable.toList (vertexes graph)
    let w = Map.fromList $ (\v -> if v == s then (value v, 0) else (value v, 100000)) <$> list
    let e = Map.fromList $ (\v -> (value v, False)) <$> list
    let q = PQ.singleton (DState s 0)
    debug $ show w
    debug $ show e
    debug $ show q
    put $ DWork w e q (Map.empty)
    whileM_ (not <$> isEmpty) $ do
        loop graph
    work <- get
    debug $ "Total " ++ show (weights work)
    return []

loop :: FlowGraph -> StateT DWork IO ()
loop graph = do
    (DWork w e q c) <- get
    let (DState current _) = PQ.findMin q
    debug $ "Current vertex: " ++ (value current)
    let (Just visited) = Map.lookup (value current) e
    put $ DWork w e (PQ.deleteMin q) c
    if visited
        then do
            debug $ "Already visited"
        else do
            let neighbours = successors current
            for_ neighbours $ \edge -> do
                debug $ "Iteration for " ++ show edge
                iteration graph edge
                (DWork w _ _ _) <- get
                debug $ show w

iteration :: FlowGraph -> FlowEdge -> StateT DWork IO ()
iteration graph edge = do
    let current = from edge
    let neighbour = to edge
    (DWork w e q c) <- get
    let (Just currentW) = Map.lookup current w
    let (cost, flow) = weight edge
    let (Just neighbourW) = Map.lookup neighbour w
    let (Just visited) = Map.lookup neighbour e
    let tentativeW = currentW + flow
    --debug $ show currentW ++ " - " ++ show cost ++ " " ++ show neighbourW ++ " " ++ show visited
    if (not visited) && (tentativeW < neighbourW)
        then do
            debug $ "Put to queue " ++ show neighbour
            put $ DWork (Map.insert neighbour tentativeW w) e (PQ.insert (DState (getVertex graph neighbour) tentativeW) q) c
        else return ()

maxFlow :: FlowGraph -> String -> String -> StateT DWork IO ()
maxFlow graph source target = do
    let s = getVertex graph source
    let t = getVertex graph target

    --helper graph s t
    path <- dijkstra graph s t
    return ()

