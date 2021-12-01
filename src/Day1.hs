module Day1
    ( solve1
    , solve2
    ) where

import Data.List (foldl')

parseInput :: String -> [Int]
parseInput = (map read) . words

solve1 :: String -> String
solve1 = show . depth1 . parseInput

solve2 :: String -> String
solve2 = show . depth2 . parseInput

--
-- Funner :)
--

depth1 :: [Int] -> Int
depth1 xs = sum . map (fromEnum . uncurry (>)) $ zip (tail xs) xs

depth2 :: [Int] -> Int
depth2 xs = depth1 $ map (uncurry3 (+)) $ zip3 (tail (tail xs)) (tail xs) xs
  where
      uncurry3 f (x,y,z) = (x `f` y `f` z)

--
-- Clear and easy solutions
--

recursive :: [Int] -> Int
recursive (x:y:xs)
  | x < y     = 1 + recursive (y:xs)
  | otherwise =     recursive (y:xs)
recursive _      = 0

recursive2 :: Int -> [Int] -> Int
recursive2 n (x:y:z:rest)
  | n < (x+y+z) = 1 + recursive2 (x+y+z) (y:z:rest)
  | otherwise   =     recursive2 (x+y+z) (y:z:rest)
recursive2 _ _      = 0
