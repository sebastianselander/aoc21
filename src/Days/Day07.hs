module Days.Day07
    ( solve1
    , solve2
    ) where

import Misc

solve1 :: String -> Int
solve1 str = minimum $ allFuel (fuel id) (interval p) p
  where
    p = parseInput str

solve2 :: String -> Int
solve2 str = minimum $ allFuel (fuel num) (interval p) p
  where
    p = parseInput str

parseInput :: String -> [Int]
parseInput = map read . splitOn ','

interval :: [Int] -> [Int]
interval xs = [minimum xs .. maximum xs]

fuel :: (Int -> Int) -> Int -> [Int] -> Int
fuel n = sum . map (f . abs . (n -))

allFuel :: (Int -> [Int] -> Int) -> [Int] -> [Int] -> [Int]
allFuel f xs ys = map (`f` ys) xs

num :: Int -> Int
num n = n*(n+1) `div` 2
