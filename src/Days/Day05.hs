module Days.Day05
    ( solve1
    , solve2
    ) where

import Misc
import qualified Data.Map as M

-- kinda cringe nubbing
solve1 :: String -> Int
solve1 = length . nub . allOverlap . filter (not . diagonal) . parseInput

-- even more cringe here
solve2 :: String -> Int
solve2 = length . nub . allOverlap . nub . parseInput

data Line a = Line (a, a) (a, a)
    deriving (Show, Eq)

data Orientation = Clockwise | Counterclockwise | Collinear
    deriving (Show, Eq)

toLine :: Int -> Int -> Int -> Int -> Line Int
toLine a b c d = Line (a,b) (c,d)

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

findOrientation :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Orientation
findOrientation (a1,a2) (b1,b2) (c1,c2) = 
  case signum $ (b2 - a2) * (c1 - b1) - (b1 - a1) * (c2 - b2) of
      0    -> Collinear
      (-1) -> Counterclockwise
      1    -> Clockwise
      _ -> error "can't find orientation"

overlap :: Line Int -> Line Int -> Bool
overlap (Line (x1,y1) (x2,y2)) (Line (a1,b1) (a2,b2)) = 
   (findOrientation (x1,y1) (x2,y2) (a1,b1) /= 
    findOrientation (x1,y1) (x2,y2) (a2,b2) && 
    findOrientation (a1,b1) (a2,b2) (x1,y1) /= 
    findOrientation (a1,b1) (a2,b2) (x2,y2))`xor`
   (Collinear == findOrientation (x1,y1) (x2,y2) (a1,b1) && 
    Collinear == findOrientation (x1,y1) (x2,y2) (a2,b2) && 
    Collinear == findOrientation (a1,b1) (a2,b2) (x1,y1) && 
    Collinear == findOrientation (a1,b1) (a2,b2) (x2,y2) &&
    (not . null $ intersect [min x1 x2 .. max x1 x2] [min a1 a2 .. max a1 a2]) &&
    (not . null $ intersect [min y1 y2 .. max y1 y2] [min b1 b2 .. max b1 b2])
    ) 
    where
      xor a b = (a && not b) || (not a && b)

lineToPoints :: Line Int -> [(Int, Int)]
lineToPoints l@(Line (x1,y1) (x2,y2)) 
  | diagonal l = zip [x1, x1 + signum (x2 - x1) .. x2]
                     [y1, y1 + signum (y2 - y1) .. y2]
  | otherwise  = [(xs,ys) | xs <- [min x1 x2 .. max x1 x2],
                            ys <- [min y1 y2 .. max y1 y2]]

getOverlappingPoints :: Line Int -> Line Int -> [((Int,Int), Int)]
getOverlappingPoints l1 l2
  | overlap l1 l2 = 
      filter ((>=2) . snd) 
    . map (\x -> (head x, length x)) 
    . group 
    . sort $ lineToPoints l1 <> lineToPoints l2
  | otherwise     = []

lineOverlapGrid :: Line Int -> [Line Int] -> [(Int, Int)]
lineOverlapGrid l = map fst . concatMap (getOverlappingPoints l)

allOverlap :: [Line Int] -> [(Int, Int)]
allOverlap = concat . allOverlap'
  where
    allOverlap' :: [Line Int] -> [[(Int, Int)]]
    allOverlap' []     = [] 
    allOverlap' (x:xs) = lineOverlapGrid x xs : allOverlap' xs
