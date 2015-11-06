module B3.EdmondsKarp(readGraph, writeGraph, bfs, maxFlow) where

import qualified Data.Map.Lazy as Map
import Data.Functor
import B3.Graph
import System.IO
import Data.Monoid
import Data.Foldable(for_, toList, foldl, minimumBy)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified Data.Dequeue as Dequeue
import Control.Monad.Loops (whileM_)
--import qualified Data.PQueue.Min as PQ

type FlowGraph = Graph String (Int, Int)
type FlowEdge = Edge String (Int, Int)
type FlowVertex = Vertex String (Int, Int)
type FlowPath = [FlowEdge]

data DState = DState FlowVertex Int
    deriving (Show)

instance Eq (DState) where
    (DState _ p1) == (DState _ p2) = p1 == p2

instance Ord (DState) where
    (DState _ p1) <= (DState _ p2) = p1 <= p2

data BFSWork = BFSWork {weights :: Map.Map String Int, expanded :: Map.Map String Bool, queue :: Dequeue.BankersDequeue DState, cameFrom :: Map.Map String FlowEdge}
    deriving (Show)

data EKWork = EKWork {getGraph :: FlowGraph, isStop :: Bool}
    deriving (Show)

readEdge :: String -> FlowEdge
readEdge s = Edge a b (0, c) where
    slices = words s
    a = slices !! 0
    b = slices !! 1
    c = read (slices !! 2) :: Int

readGraph :: FilePath -> IO FlowGraph
readGraph filename = do
    content <- readFile filename
    let graph = Data.Foldable.foldl folder (Graph Map.empty) (readEdge <$> lines content) where
        folder g (Edge u v (_, c)) = g5 where
            g2 = initVertex g u
            g3 = initVertex g2 v
            g4 = putEdge g3 (Edge u v (0, c))
            g5 = putEdge g4 (Edge v u (0, 0))
    return graph

writeEdge :: FlowEdge -> String
writeEdge (Edge a b (c, d)) = a ++ " " ++ b ++ " " ++ show c ++ "/" ++ show d

writeVertex :: FlowVertex -> [String]
writeVertex v = Map.foldl (\s e -> (writeEdge e):s) mempty (successors v)

writeGraph :: FlowGraph -> FilePath -> IO ()
writeGraph graph filename = do
    handle <- openFile filename AppendMode
    let content = unlines $ join (writeVertex <$> (Map.elems $ vertexes graph))
    hPutStrLn handle content
    hClose handle

debug :: (MonadIO m) => String -> m ()
debug x = (liftIO . putStrLn) x

--- Для справки ---
---whileM_ :: (Monad m) => m Bool -> m a -> m ()
---whileM_ p f = go
---    where go = do
---            x <- p
---            if x
---                then f >> go
---                else return ()

--- Проверка, что очередь BFS пуста ---
isEmpty :: StateT BFSWork IO Bool
isEmpty = do
    work <- get
    return $ Dequeue.null (queue work)

--- Поиск в ширину ---
bfs :: FlowGraph -> FlowVertex -> FlowVertex -> IO FlowPath
bfs graph s t = evalStateT (bfs' graph s t) (BFSWork Map.empty Map.empty Dequeue.empty Map.empty)

bfs' :: FlowGraph -> FlowVertex -> FlowVertex -> StateT BFSWork IO FlowPath
bfs' graph s t = do
    let list = Data.Foldable.toList (vertexes graph)
    let w = Map.fromList $ (\v -> if v == s then (value v, 0) else (value v, 100000)) <$> list
    let e = Map.fromList $ (\v -> (value v, False)) <$> list
    let q = Dequeue.fromList [(DState s 0)]
    --debug $ show w
    --debug $ show e
    --debug $ show q
    put $ BFSWork w e q (Map.empty)
    whileM_ (not <$> isEmpty) $ do
        loop graph
    work <- get
    --debug $ "Total " ++ show (cameFrom work)
    return $ reverse (buildPath (cameFrom work) (value t))

--- Восстановление пути ---
buildPath :: Map.Map String FlowEdge -> String -> FlowPath
buildPath oldC v = do
    case Map.lookup v oldC of
        Nothing     -> []
        Just edge   -> edge:(buildPath oldC (from edge))

--- Основной цикл BFS ---
loop :: FlowGraph -> StateT BFSWork IO ()
loop graph = do
    (BFSWork oldW oldE oldQ oldC) <- get
    let Just (DState current _, newQ) = Dequeue.popFront oldQ
    let newE = Map.insert (value current) True oldE
    put $ BFSWork oldW newE newQ oldC
    ---debug $ "Current vertex: " ++ (value current)
    let (Just visited) = Map.lookup (value current) oldE
    if visited
        then do
            debug $ "Already visited"
        else do
            let neighbours = successors current
            for_ neighbours $ \edge -> do
                --debug $ "Iteration for " ++ show edge
                iteration graph edge
                ---(DWork w _ _ _) <- get
                ---debug $ show w

--- Итерация одного исходящего ребра ---
iteration :: FlowGraph -> FlowEdge -> StateT BFSWork IO ()
iteration graph edge = do
    let current = from edge
    let neighbour = to edge
    (BFSWork oldW oldE oldQ oldC) <- get
    let (Just currentW) = Map.lookup current oldW
    let (Just neighbourW) = Map.lookup neighbour oldW
    let (cost, flow) = weight edge
    let (Just visited) = Map.lookup neighbour oldE
    let tentativeW = currentW + 1
    --debug $ show currentW ++ " - " ++ show cost ++ " " ++ show neighbourW ++ " " ++ show visited
    if (not visited) && (tentativeW < neighbourW) && (flow - cost > 0)
        then do
            let newW = Map.insert neighbour tentativeW oldW
            let newQ = Dequeue.pushBack oldQ $ DState (getVertex graph neighbour) tentativeW
            let newC = Map.insert neighbour edge oldC
            --debug $ "Put to queue " ++ show neighbour
            put $ BFSWork newW oldE newQ newC
        else return ()

--- Поиск максимального потока ---
maxFlow :: FlowGraph -> FlowVertex -> FlowVertex -> IO Int
maxFlow graph source target = evalStateT (maxFlow' source target) (EKWork graph False)

--- Проверка флага, что должны выйти из цикла ---
shouldStop :: StateT EKWork IO Bool
shouldStop = do
    work <- get
    return $ isStop work

--- Вычисление минимальной пропускной способности пути в остаточной сети ---
minFlow :: FlowPath -> Int
minFlow path = diff (weight $ minimumBy comparator path) where
    diff (c, f) = f - c
    comparator (Edge _ _ cf1) (Edge _ _ cf2) = compare (diff cf1) (diff cf2)

--- Обновление потока по текущему ребру и противонапревленному ему ---
flowIteration :: FlowEdge -> Int -> StateT EKWork IO ()
flowIteration (Edge u v (c, f)) minC = do
    EKWork oldG oldS <- get
    let tempG = putEdge oldG (Edge u v (c + minC, f))
    let (Edge _ _ (bc, bf)) = getEdge tempG v u
    let newG = putEdge tempG (Edge v u (bc - minC, bf))
    put $ EKWork newG oldS

--- Основной цикл алгоритма Эдмондса - Карпа ---
maxFlow' :: FlowVertex -> FlowVertex -> StateT EKWork IO Int
maxFlow' source target = do
    whileM_ (not <$> shouldStop) $ do
        EKWork oldG _ <- get
        let s = getVertex oldG (value source)
        let t = getVertex oldG (value target)
        --debug $ "Old Graph: " ++ show oldG
        path <- liftIO $ bfs oldG s t
        --debug $ "Path: " ++ show path
        if null path
            then do
                put $ EKWork oldG True
            else do
                let minC = minFlow path
                --debug $ "MinFlow: " ++ show minC
                forM_ path $ \edge -> do
                    flowIteration edge minC
                --EKWork newG _ <- get
                --debug $ "Graph: " ++ show newG
    EKWork totalG _ <- get
    let edges = successors (getVertex totalG (value source))
    --debug $ show sourceNeighbours
    let totalFlow = Map.foldl (\acc edge -> acc + (fst $ weight edge)) 0 edges
    return totalFlow


