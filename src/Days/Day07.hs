module Days.Day07
    ( solve1
    , solve2
    ) where

import Misc
import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 = ((sum .) =<< map . (abs .) . (-) . median) . parseInput


solve2 :: String -> Int
solve2 = ((sum .) =<< map . ((num . abs) .) . (-) . average) . parseInput
parseInput :: String -> [Int]
parseInput = map read . splitOn ','

average :: [Int] -> Int
average = uncurry div . (sum &&& length) 

median :: [Int] -> Int
median xs = (sort xs) !! (length xs `div` 2)

num :: Int -> Int
num n = n * (n + 1) `div` 2
