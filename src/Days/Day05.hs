module Days.Day05
    ( solve1
    , solve2
    ) where

import Misc

solve1 :: String -> Int
solve1 = getOverlaps . filter (not . diagonal) . parseInput 

solve2 :: String -> Int
solve2 = getOverlaps . parseInput 

data Line a = Line (a, a) (a, a)
    deriving (Show, Eq)

parseInput :: String -> [Line Int]
parseInput = map (toLine . map ((map read) . splitOn ',') . words . splitString) . lines
  where
    toLine :: [[a]] -> Line a
    toLine [[a,b],[c,d]] = Line (a,b) (c,d)
    toLine _             = error "failed converting to line"
    splitString :: String -> String
    splitString ""             = ""
    splitString (x:xs) 
      | x `elem` "1234567890, " = x : splitString xs
      | otherwise               =     splitString xs

diagonal :: Line Int -> Bool
diagonal (Line (x1,y1) (x2,y2)) = x1 /= x2 && y1 /= y2

lineToPoints :: Line Int -> [(Int, Int)]
lineToPoints l@(Line (x1,y1) (x2,y2)) 
  | diagonal l = zip [x1, x1 + signum (x2 - x1) .. x2]
                     [y1, y1 + signum (y2 - y1) .. y2]
  | otherwise  = [(xs,ys) | xs <- [min x1 x2 .. max x1 x2],
                            ys <- [min y1 y2 .. max y1 y2]]

getOverlaps :: [Line Int] -> Int
getOverlaps = length . filter ((>=2) . length) . group . sort . concat . map lineToPoints 
