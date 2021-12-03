module Days.Day2
    ( solve1
    , solve2
    ) where

import Misc (parseAsStringGenList)

solve1 :: String -> Int
solve1 = path (0,0) . parseAsStringGenList

solve2 :: String -> Int
solve2 = path2 0 (0,0) . parseAsStringGenList

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





