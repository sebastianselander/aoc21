module Day1
    ( solve1
    , solve2
    ) where


parseInput :: String -> [Int]
parseInput = (map read) . words

solve1 :: String -> String
solve1 = show . depth1 . parseInput

solve2 :: String -> String
solve2 = show . (depth2 maxBound) . parseInput

depth1 :: [Int] -> Int
depth1 (x:y:xs)
  | x < y     = 1 + depth1 (y:xs)
  | otherwise =     depth1 (y:xs)
depth1 _ = 0

depth2 :: Int -> [Int] -> Int
depth2 n (x:y:z:rest)
  | n < (x+y+z) = 1 + depth2 (x+y+z) (y:z:rest)
  | otherwise   =     depth2 (x+y+z) (y:z:rest)
depth2 _ _      = 0
