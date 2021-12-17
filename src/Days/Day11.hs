module Days.Day11
    ( solve1
    , solve2
    ) where

import Misc
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 = sumSteps . steps 100 . map toOctopus . parseInput

solve2 :: String -> Int
solve2 = steps2 0 . (const 0 &&& (map toOctopus . parseInput))

data Octopus = O 
             { flashed :: Bool
             , value :: Int
             } deriving Eq

instance Show Octopus where
  show (O b v) = show v

toOctopus :: Int -> Octopus
toOctopus n = O { flashed = False, value = n }

parseInput :: String -> [Int]
parseInput = concatMap (map (read . (:""))) . lines

add :: Int -> Octopus -> Octopus
add n (O b v) = O b (v + n) 

-- this is quite dirty
size :: [a] -> Int
size = round . sqrt . fromIntegral . length 

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
  | i < 0          = Nothing
  | i >= length xs = Nothing
  | otherwise      = Just (xs !! i)

neighborIndexes :: Int -> Int -> [Int]
neighborIndexes sz n 
  | (n + 1) `mod` sz == 0 = [n - sz - 1 , n - sz , n + sz , n + sz - 1 , n - 1]
                            -- don't check points to the right if next to the wall
  | n `mod` sz       == 0 = [n - sz , n - sz + 1 , n + 1 , n + sz + 1 , n + sz]
                            -- don't check points to the left if next to the wall
  | otherwise             = [n - sz - 1 , n - sz , n - sz + 1 , n + 1
                            , n + sz + 1 , n + sz , n + sz - 1 , n - 1]
                            -- all points around 

indexesToOctopi :: Int -> [Octopus] -> [Octopus]
indexesToOctopi idx grid 
    = mapMaybe (`safeIndex` grid) (neighborIndexes (size grid) idx)

poppableOctopi :: Int -> [Octopus] -> Int
poppableOctopi idx = length . filter okOctopus . indexesToOctopi idx
  where
    okOctopus (O b v) = not b && v > 9

partialStep :: [Octopus] -> [Octopus]
partialStep = uncurry (`go` 0) . (length &&& id)
  where
    go :: Int -> Int -> [Octopus] -> [Octopus]
    go cap idx xs | cap == idx = []
    go cap idx xs = case xs !! idx of
        (O False n) -> if n > 9 then O True n : go cap (idx + 1) xs
                       else add (poppableOctopi idx xs) (xs !! idx) 
                                : go cap (idx + 1) xs
        octo        ->  add (poppableOctopi idx xs) octo : go cap (idx + 1) xs

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
      | buf == grid = convertPopped (partialStep grid)
      | otherwise   = go (partialStep grid) (partialStep (partialStep grid))

popped :: [Octopus] -> Int
popped = foldl' (\acc (O b v) -> if v == 0 then acc + 1 else acc) 0

steps :: Int -> [Octopus] -> [(Int,[Octopus])]
steps 0    grid = []
steps iter grid = step grid : steps (iter - 1) (snd $ step grid)

sumSteps :: [(Int, [Octopus])] -> Int
sumSteps = sum . map fst 

allFlash :: [Octopus] -> Bool
allFlash = uncurry (==) . (length &&& popped)

steps2 :: Int -> (Int,[Octopus]) -> Int
steps2 n (v, grid)
  | allFlash grid = n
  | otherwise     = steps2 (n + 1) (step grid)
