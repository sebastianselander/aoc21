module Days.Day02
    ( solve1
    , solve2
    ) where

import           Misc

solve1 :: String -> Int
solve1 = path (0,0) . parseInput

solve2 :: String -> Int
solve2 = path2 0 (0,0) . parseInput

parseInput :: Read a => String -> [(String,a)]
parseInput = map (second read. toTuple . words) . lines
  where
    toTuple :: [String] -> (String,String)
    toTuple [x,y] = (x,y)

path :: (Int,Int) -> [(String,Int)] -> Int
path (x,y) []         = x*y
path (x,y) ((inst,n):ab) = case inst of
                      "forward" -> path (x+n,y) ab
                      "up"      -> path (x,y-n) ab
                      "down"    -> path (x,y+n) ab


path2 :: Int -> (Int,Int) -> [(String,Int)] -> Int
path2 aim (x,y) []         = x * y
path2 aim (x,y) ((inst,n):ab) =
  case inst of
    "forward" -> path2 aim     (x + n, y + (n * aim) ) ab
    "up"      -> path2 (aim - n) (x,y) ab
    "down"    -> path2 (aim + n) (x,y) ab





