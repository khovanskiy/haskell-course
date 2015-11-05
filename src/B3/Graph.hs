module B3.Graph(Graph(..), Edge(..), Vertex(..), initVertex, putVertex, putEdge, Map.empty) where

import qualified Data.Map.Lazy as Map

data Edge a b = Edge {from :: a, to :: a, weight :: b}
    deriving (Eq, Ord, Show)

data Vertex a b = Vertex {value :: a, successors :: Map.Map (a, a) (Edge a b)}
    deriving (Show)

data Graph a b = Graph {vertexes :: Map.Map a (Vertex a b)}
    deriving (Show)

initVertex :: (Ord a) => Graph a b -> a -> Graph a b
initVertex g v = case Map.lookup v (vertexes g) of
    Nothing -> Graph $ Map.insert v (Vertex v Map.empty) (vertexes g)
    _       -> g

putVertex :: (Ord a) => Graph a b -> Vertex a b -> Graph a b
putVertex g v = Graph $ Map.insert (value v) v (vertexes g)

addEdge' :: (Ord a) => Vertex a b -> Edge a b -> Vertex a b
addEdge' (Vertex v vs) (Edge f t c) = do
    Vertex v (Map.insert (f, t) (Edge f t c) vs)

putEdge :: (Ord a) => Graph a b -> Edge a b -> Graph a b
putEdge g e = case Map.lookup (from e) (vertexes g) of
    Nothing -> error $ "Vertex not found"
    Just v  -> putVertex g $ addEdge' v e






