module Days.Day15
    ( solve1
    , solve2
    ) where

import Misc
import qualified Data.IntMap as M
import qualified Graph as G
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import Data.Maybe (fromJust)

solve1 :: String -> Int
solve1 str = snd . fromJust $ p1 0 9999 $ edges 0 100 (parseInput 100 str) (vertices 100)

solve2 :: String -> Int
solve2 str = snd . fromJust $ p1 0 249999 $ edges 0 500 (parseInput2 500 str) (vertices 500)

d15 :: String
d15 = unsafePerformIO $ readFile "input/Day15.txt"

t15 :: String
t15 = unsafePerformIO $ readFile "input/Day15test.txt"

parseInput :: Int -> String -> M.IntMap Int
parseInput width str = go width 0 (concatMap (map (read . (:""))) $ lines str) M.empty
  where
    go :: Int -> Int -> [Int] -> M.IntMap Int -> M.IntMap Int
    go width n _      m | n == width * width = m 
    go width n (x:xs) m = go width (n + 1) xs (M.insert n x m)

parseInput2 :: Int -> String -> M.IntMap Int
parseInput2 width str = go width 0 (concat $ transpose $ map extendRow $ transpose $ map (extendRow . map (read . (:""))) $ lines str) M.empty
  where
    go :: Int -> Int -> [Int] -> M.IntMap Int -> M.IntMap Int
    go width n _      m | n == width * width = m 
    go width n (x:xs) m = go width (n + 1) xs (M.insert n x m)
    extendRow :: [Int] -> [Int]
    extendRow = concat . take 5 . iterate (map ((+1) . flip mod 9))

neighbIdx :: Int -> Int -> [Int]
neighbIdx width n 
  | (n + 1) `mod` width == 0 = outOfBounds [n - 1, n + width, n - width]
  | n `mod`       width == 0 = outOfBounds [n + 1, n + width, n - width]
  | otherwise                = outOfBounds [n + 1, n - 1, n + width, n - width]
  where
    outOfBounds = filter (\x -> x >= 0 && x < width^2) 

vertices :: Int -> G.Graph Int b
vertices n = G.fromVertexList [0..(n * n - 1)]

edges :: Int -> Int -> M.IntMap Int -> G.Graph Int Int -> G.Graph Int Int
edges n width xs g | n == width * width = g
edges n width xs g = edges (n + 1) width xs (subGraph g (neighbIdx width n))
  where
    subGraph gr []     = gr
    subGraph gr (a:as) = subGraph (G.addEdge n a (xs M.! a) gr) as

p1 :: Int -> Int -> G.Graph Int Int -> Maybe ([Int], Int)
p1 from to g = G.findPath from to g
