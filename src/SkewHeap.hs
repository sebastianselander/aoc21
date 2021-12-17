{-# LANGUAGE TupleSections #-}

module SkewHeap
  ( SkewHeap
  , empty
  , singleton
  , fromList
  , merge
  , insert
  , removeMin
  , peek
  , takeMin
  , remove
  , replace
  , toList
  , isEmpty
  , size
  , depth
  ) where

import           Data.Function                  ( on )
import           Data.List                      ( sort )

-- | Type constructor for a skew heap.
data SkewHeap a = Empty
                | Node (SkewHeap a) a (SkewHeap a)
   deriving (Show)


-- | Creating skew heaps.

-- | Creates an empty skew heap; O(1).
empty :: SkewHeap a
empty = Empty

-- | Creates a skew heap with a single element; O(1).
singleton :: Ord a => a -> SkewHeap a
singleton x = Node Empty x Empty

-- | Creates a skew heap from a list; O(n^2), amortized O(n log n).
fromList :: Ord a => [a] -> SkewHeap a
fromList = foldl (flip insert) Empty


-- | Doing stuff to heaps.

-- | Merges two skew heaps; O(n), amortized O(log n).
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge h     Empty = h
merge Empty h     = h
merge t1@(Node l1 e1 r1) t2@(Node l2 e2 r2)
  | e1 < e2   = Node (merge r1 t2) e1 l1
  | otherwise = Node l2 e2 (merge r2 t1)

-- | Inserts an element into the heap; O(n), amortized O(log n).
insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert e = merge (singleton e)

-- | Removes the root element of the skew heap; O(n), amortized O(log n).
removeMin :: Ord a => SkewHeap a -> SkewHeap a
removeMin Empty        = Empty
removeMin (Node l _ r) = merge l r

-- | Returns the root element of the skew heap, maybe; O(1).
peek :: SkewHeap a -> Maybe a
peek Empty        = Nothing
peek (Node _ x _) = Just x

-- | Takes the root element out of the skew heap and maybe returns it and the
-- rest of the heap; O(n), amortized O(log n).
takeMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
takeMin Empty = Nothing
takeMin h     = (, removeMin h) <$> peek h

-- | Removes all elements equal (==) to the specified element; O(n^2),
-- amortized O(n log n), even more amortized O(log n) (unless a large portion
-- of the elements are being removed).
remove :: Ord a => a -> SkewHeap a -> SkewHeap a
remove _ Empty = Empty
remove x (Node l y r) | x > y     = Node l y r
                      | x == y    = merge (remove x l) (remove x r)
                      | otherwise = Node (remove x l) y (remove x r)

-- | Replaces all elements equal to r with one instance of i; O(n),
-- amortized O(log n).
replace :: Ord a => a -> a -> SkewHeap a -> SkewHeap a
replace r i = insert i . remove r

-- | Creates a list from a skew heap; O(n^2), amortized O(n log n).
toList :: Ord a => SkewHeap a -> [a]
toList = maybe [] (\(e, h) -> e : toList h) . takeMin


-- | Getting information about heaps.

-- | Returns True if a skew heap is empty; O(1).
isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Counts the number of elements in a skew heap; O(n^2), amortized O(n log n).
size :: Ord a => SkewHeap a -> Int
size = maybe 0 (\(_, h) -> 1 + size h) . takeMin

-- | Calculates the depth of a skew heap; O(n^2), amortized O(n log n).
depth :: Ord a => SkewHeap a -> Int
depth Empty        = 0
depth (Node l _ r) = 1 + (max `on` depth) l r

-- | Verify that the invariant holds; O(n^2), amortized O(n log n).
invariant :: Ord a => SkewHeap a -> Bool
invariant Empty          = True
invariant h@(Node _ x _) = invariant' x h
 where
  invariant' _ Empty         = True
  invariant' m (Node l x' r) = m <= x' && invariant' x' l && invariant' x' r
