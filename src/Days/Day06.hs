module Days.Day06
    ( solve1
    , solve2
    ) where

import Misc

solve1 :: String -> Int
solve1 = nSteps 80 . parseInput

solve2 :: String -> Int
solve2 = nSteps 256 . parseInput

parseInput :: String -> [Int]
parseInput = map (length . tail) 
           . group 
           . sort 
           . map (read :: String -> Int) 
           . splitOn ',' 
           . ("0,1,2,3,4,5,6,7,8," ++)

step :: [Int] -> [Int]
step [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h+a,i,a]
step _                   = error "can't step"

nSteps :: Int -> [Int] -> Int
nSteps n = sum . last . take (n + 1) . iterate step
