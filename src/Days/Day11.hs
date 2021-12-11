module Days.Day11
    ( solve1
    , solve2
    ) where

import Misc
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (catMaybes)
import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 = sumSteps . steps 100 . map toOctopus . parseInput

solve2 :: String -> Int
solve2 str = steps2 0 (0, map toOctopus $ parseInput $ str)

data Octopus = O 
             { flashed :: Bool
             , value :: Int
             } deriving Eq

size :: Int 
size = 10

toOctopus :: Int -> Octopus
toOctopus n = O {flashed = False, value = n}

parseInput :: String -> [Int]
parseInput = concatMap (map (read . (:""))) . lines

add :: Int -> Octopus -> Octopus
add n (O b v) = O b (v+n) 

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
  | i < 0          = Nothing
  | i >= length xs = Nothing
  | otherwise      = Just (xs !! i)

neighborIndexs :: Int -> [Int]
neighborIndexs n 
  | (n + 1) `mod` size == 0 = [n+size, n-size, n-1, n+size-1, n-size-1]
  | n `mod` size       == 0 = [n+size, n-size, n+1, n+size+1, n-size+1]
  | otherwise = [n+1,n-1, n+size,n-size ,n+size+1,n+size-1 ,n-size+1,n-size-1]

indexesToOctopi :: Int -> [Octopus] -> [Octopus]
indexesToOctopi idx grid = catMaybes $ map (flip safeIndex grid) (neighborIndexs idx)

poppableOctopi :: Int -> [Octopus] -> Int
poppableOctopi idx grid = length $ filter (okOctopus) $ indexesToOctopi idx grid
  where
    okOctopus (O b v) = not b && v > 9

partialStep :: [Octopus] -> [Octopus]
partialStep = go (size*size) 0 
  where
    go :: Int -> Int -> [Octopus] -> [Octopus]
    go cap idx xs | cap == idx = []
    go cap idx xs = case xs !! idx of
        (O False n) -> if (n > 9) then (O True n) : go cap (idx + 1) xs
                       else (add (poppableOctopi idx xs) (xs !! idx)) : go cap (idx + 1) xs
        octo        ->  (add (poppableOctopi idx xs) octo) : go cap (idx + 1) xs

convertPopped :: [Octopus] -> [Octopus]
convertPopped = map convert
  where
    convert o@(O b v)
      | b || v > 9 = O False 0
      | otherwise  = o

step :: [Octopus] -> (Int, [Octopus])
step xs = (popped &&& id) (go [] (map (add 1) xs))
  where
    go :: [Octopus] -> [Octopus] -> [Octopus]
    go buf grid
      | buf == partialStep grid = convertPopped (partialStep grid)
      | otherwise               = go (partialStep grid) (partialStep grid)

popped :: [Octopus] -> Int
popped = foldl' (\acc (O b v) -> if v==0 then acc + 1 else acc) 0

steps :: Int -> [Octopus] -> [(Int,[Octopus])]
steps 0    grid = []
steps iter grid = step grid : steps (iter-1) (snd $ step grid)

sumSteps :: [(Int, [Octopus])] -> Int
sumSteps = sum . map fst 

allFlash :: [Octopus] -> Bool
allFlash = uncurry (==) . (length &&& popped)

steps2 :: Int -> (Int,[Octopus]) -> Int
steps2 n (v,grid)
  | allFlash grid = n
  | otherwise     = steps2 (n+1) (step grid)
