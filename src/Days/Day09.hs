module Days.Day09
    ( solve1
    , solve2
    ) where

import Misc
import Data.Maybe (fromJust)
import qualified Data.Map as M

solve1 :: String -> Int
solve1 = sum 
       . map (+1) 
       . fromJust 
       . sequenceA 
       . map snd 
       . lows 100 0 
       . parseInput

solve2 :: String -> Int
solve2 = product 
       . take 3 
       . sortBy (\x y -> if x > y then LT else GT) 
       . map ((+1) . length) 
       . recursively 100 
       . parseInput

parseInput :: String -> [Int]
parseInput = concat 
           . concatMap (map (map read . words . intersperse ' ') . words) 
           . lines

lows :: Int -> Int ->  [Int] -> [(Index,Maybe Int)]
lows rowSize index xs | index >= length xs = []
lows rowSize index xs 
  | current `lt` before &&
    current `lt` after && 
    current `lt` above && 
    current `lt` below = (index,current) : lows rowSize (index + 1) xs
  | otherwise          =  lows rowSize (index + 1) xs
  where
    current = safeIndex index             xs
    before  = safeIndex (index - 1)       xs
    after   = safeIndex (index + 1)       xs
    above   = safeIndex (index - rowSize) xs
    below   = safeIndex (index + rowSize) xs
    lt :: Maybe Int -> Maybe Int -> Bool
    lt (Just x) (Just y) = x < y
    lt _        _        = True

safeIndex :: Int -> [Int] -> Maybe Int
safeIndex i xs
  | i < 0          = Nothing
  | i >= length xs = Nothing
  | otherwise      = Just (xs !! i)

--------------------------------- PART 2 ---------------------------------------

type Value = Int
type Index = Int

toMap :: [a] -> M.Map Int a
toMap = toMap' 0
  where
    toMap' :: Int -> [a] -> M.Map Int a
    toMap' n []     = M.empty
    toMap' n (x:xs) = M.insert n x (toMap' (n + 1) xs)

neighborIndex :: Index -> Int -> [Int]
neighborIndex n edge 
  | (n + 1) `mod` edge == 0 = [n - edge, n - 1, n + edge]
  | n `mod` edge       == 0 = [n - edge, n + 1, n + edge]
  | otherwise               = [n - edge, n - 1, n + 1, n + edge]

getAll :: Index -> Int -> M.Map Int Int -> [(Index, Maybe Value)]
getAll index edge m = 
    filter (\(a,b) -> diff current b) 
    $ map (\x -> (x, M.lookup x m)) (neighborIndex index edge)
  where
    current :: Maybe Int
    current = M.lookup index m
    diff :: Maybe Int -> Maybe Int -> Bool
    diff (Just a) (Just b) = a < b
    diff _        _        = False

recursively :: Int -> [Int] -> [[(Index, Maybe Value)]]
recursively edge l = map (nub . filter (\(a, Just x) -> x /= 9) 
                   . concat 
                   . filter (not . null) 
                   . recursively' edge (toMap l)) indexes
  where
    indexes = map fst (lows edge 0 l)

recursively' :: Int -> M.Map Int Int -> Index -> [[(Index, Maybe Value)]]
recursively' edge m index = 
    let gotten = getAll index edge m 
    in  gotten : concatMap (recursively' edge m . fst) gotten
