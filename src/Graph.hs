module Graph
  ( Graph
  , empty
  , edge
  , graph
  , addEdge
  , addVertex
  , vertices
  , size
  , adjacent
  , dijkstra
  , fromVertexList
  , fromEdgeList
  , findPath
  )
  where

import Data.Function (on)
import Data.List (find)
import Data.Map (Map)
import SkewHeap (SkewHeap)
import Data.Maybe
import qualified Data.Map as M
import qualified SkewHeap as S

data Edge a b = Edge
  { source :: a
  , destination :: a
  , weight :: b
  } deriving Show

data Graph a b = Graph
  { adjMap :: Map a [Edge a b] }
  deriving Show

-- | Creates an empty graph; O(1).
empty :: Graph a b
empty = Graph {adjMap = M.empty}

-- | Creates an edge from a source, destination and a weight; O(1).
edge :: a -> a -> b -> Edge a b
edge src dest wgt = Edge { source = src, destination = dest, weight = wgt }

-- | Creates a graph from an edge; O(1).
-- M.insert is O(log n), but since we always insert into an map we get O(1).
graph :: Ord a => Edge a b -> Graph a b
graph e = Graph { adjMap = M.insert (destination e) [] $ M.singleton (source e) [e] }

-- | Compare if the source and destination of an edge are the same; O(1).
same :: Eq a => Edge a b -> Edge a b -> Bool
same e1 e2 = ((==) `on` source) e1 e2 && ((==) `on` destination) e1 e2

-- | Add an edge to a map; O(log n)
addToMap :: Ord a => Edge a b -> Map a [Edge a b] -> Map a [Edge a b]
addToMap e m = M.adjust (e:) (source e) m

-- | Return the edge with the least weight; O(1).
minWeight :: (Ord b, Eq a) => Edge a b -> Edge a b -> Edge a b
minWeight e1 e2 | not $ same e1 e2          = error "different edges compared"
                | (weight e1) < (weight e2) = e1
                | otherwise                 = e2

-- | Add a vertex to a graph; O(log n);
addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v = Graph . M.insert v [] . adjMap

-- | Create an edge from three inputs and add it to the graph.
-- If the vertices between the edge don't exist, throw an error; O(log n).
addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge s d w g
  | containsNodes = Graph { adjMap = addToMap e (adjMap g)}
  | otherwise     = error "missing vertices"
    where e = edge s d w
          containsNodes = s `M.member` (adjMap g) && d `M.member` (adjMap g)

-- | Add an edge to a graph; O(log n).
addEdgeAsEdge :: Ord a => Edge a b -> Graph a b -> Graph a b
addEdgeAsEdge e = addEdge (source e) (destination e) (weight e)

-- | Return the vertices as a list from a graph; O(n).
vertices :: Graph a b -> [a]
vertices = M.keys . adjMap

-- | Return the size of the graph; O(1).
size :: Graph a b -> Int
size = M.size . adjMap

-- | Return the adjacent vertices of a vertex
-- with the cost of the edge between them; O(log n).
adjacent :: Ord a => a -> Graph a b -> [(a, b)]
adjacent v = map (\e -> (destination e, weight e)) . concat . maybeToList . M.lookup v . adjMap

type Queue a = SkewHeap a

data DNode a b = DNode { dVertex :: a, dCost :: b, dPrev :: Maybe a }
   deriving Show

instance Eq b => Eq (DNode a b) where
  (==) = (==) `on` dCost

instance Ord b => Ord (DNode a b) where
  compare = compare `on` dCost

-- | Return the shortest paths between vertices as a map; O(|E| log |V|).
dijkstra :: (Ord a, Ord b, Num b) => a -> Graph a b -> Map a (DNode a b)
dijkstra v g = dijkstraHelper (S.singleton (DNode v 0 Nothing)) g M.empty

dijkstraHelper :: (Ord a, Ord b, Num b) =>
  Queue (DNode a b) -> Graph a b -> Map a (DNode a b) -> Map a (DNode a b)
dijkstraHelper q g m =
   case S.takeMin q of
      Nothing                        -> m
      Just (dn, q') | v `M.member` m -> dijkstraHelper q' g m
                    | otherwise      -> dijkstraHelper adj g (M.insert v dn m)
        where
            v = dVertex dn
            adj = addToQueue dn (adjacent v g ) q' m

-- | Add adjacent vertices to the queue if they aren't in the map.
addToQueue :: (Ord a, Ord b, Num b) =>
  DNode a b -> [(a, b)] -> Queue (DNode a b) -> Map a (DNode a b) -> Queue (DNode a b)
addToQueue dn xs q m = foldl add' q xs
  where
    add' dws (d, w)
      | d `M.member` m = dws
      | otherwise      = S.insert (DNode d (dCost dn + w) (Just $ dVertex dn)) dws

-- | Find the shortest path from a to b.
-- Return the path with total cost if it exists, otherwise Nothing.
findPath :: (Ord a, Ord b, Num b) => a -> a -> Graph a b -> Maybe ([a], b)
findPath from to g | null edges  = Nothing
                   | otherwise   = Just (edges, cost)
  where
    (edges, cost) = findPathHelp from (Just to) g ([], 0)

findPathHelp :: (Ord a, Ord b, Num b) => a -> Maybe a -> Graph a b -> ([a], b) -> ([a], b)
findPathHelp from to g (edges, cost)
  | isNothing to || isNothing (M.lookup (fromJust to) pathMap) = (edges, cost)
  | cost == 0 = findPathHelp from previous g ((fromJust to) : edges, edgeCost)
  | otherwise = findPathHelp from previous g ((fromJust to) : edges, cost)
  where
    pathMap = dijkstra from g
    previous = prev from to g
    edgeCost = edgeWeight from to g


-- | Find previous vertex in a path.
prev :: (Ord a, Ord b, Num b) => a -> Maybe a -> Graph a b -> Maybe a
prev from Nothing g = Nothing
prev from to g = dPrev $ fromJust  (M.lookup (fromJust to) (dijkstra from g))

-- | Find the cost of latest travelled edge in path
edgeWeight :: (Ord a, Ord b, Num b) => a -> Maybe a -> Graph a b -> b
edgeWeight from Nothing g = error "Cost of Nothing doesn't exist"
edgeWeight from to g = dCost $ fromJust (M.lookup (fromJust to) (dijkstra from g))

-- | Create a graph with vertices from a list of vertices.
fromVertexList :: Ord a => [a] -> Graph a b
fromVertexList = foldl (flip addVertex) empty

-- | Add edges from a list of edges to a graph.
fromEdgeList :: Ord a => Graph a b -> [Edge a b] -> Graph a b
fromEdgeList g = foldl (flip addEdgeAsEdge) g





testgraph :: Graph String Int
testgraph =
   addEdge "1" "2" 12 $
   addEdge "1" "3" 7 $
   addEdge "3" "4" 5 $
   addEdge "4" "2" 1 $
   addVertex "4" $
   addVertex "3" $
   addVertex "2" $
   addVertex "1" $
   Graph { adjMap = M.empty }

testgraph2 :: Graph String Int
testgraph2 =
  addEdge "1" "2" 5 $
  addEdge "1" "3" 10 $
  addEdge "1" "4" 15 $
  addEdge "2" "5" 20 $
  addEdge "3" "2" 25 $
  addVertex "1" $
  addVertex "2" $
  addVertex "3" $
  addVertex "4" $
  addVertex "5" $
  Graph { adjMap = M.empty }
