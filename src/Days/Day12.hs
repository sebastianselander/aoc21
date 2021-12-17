module Days.Day12
    ( solve1
    , solve2
    ) where

import Misc
import qualified Data.Map as M
import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 str = dfs (const True) (parseInput str) [] "start"

solve2 :: String -> Int
solve2 str = dfs dups (parseInput str) [] "start"
  where
    dups :: [String] -> Bool
    dups = uncurry (/=) . ((length . nub) &&& length)

type Graph a = M.Map a [a]

parseInput :: String -> Graph String
parseInput str = foldl' (\acc (a,b) -> M.insertWith (++) a [b] acc) M.empty parsed
  where
    parsed = filterino (parse str) ++ filterino (map (snd &&& fst) (parse str))
    filterino = filter (\(a,b) -> a /= "end" && b /= "start")
    parse = concat 
          . groupBy (\(a,b) (c,d) -> a == c) 
          . map ((\[a,b] -> (a,b)) . splitOn '-') 
          . lines

dfs :: ([String] -> Bool) -> Graph String -> [String] -> String -> Int
dfs f g visited n
  | n == "end" = 1
  | n `elem` visited && f visited = 0
  | n /= "start" && all isLower n = sum $ map (dfs f g (n:visited)) (g M.! n)
  | otherwise                     = sum $ map (dfs f g visited) (g M.! n)
