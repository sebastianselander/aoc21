module Days.Day03
    ( solve1
    , solve2
    ) where

import           Misc

type Matrix = [[Char]]

parseInput :: String -> Matrix
parseInput = parseAsMatrix -- just the lines function lol

solve1 :: String -> Int
solve1 str = gamma * epsilon
  where
    gamma = toDec . map (mostCommon (>)) . transpose . parseInput $ str
    epsilon = toDec . map (invert . mostCommon (>)) . transpose . parseInput $ str

solve2 :: String -> Int
solve2 str = co2 str * oxy str
  where
    co2 = toDec . getCO2 . parseInput
    oxy = toDec . getOxygen . parseInput

---------- Generic funcs ----------

invert :: Char -> Char
invert '1' = '0'
invert '0' = '1'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

---------- PART 1 ----------

mostCommon :: (Int -> Int -> Bool) -> String -> Char
mostCommon f str
  | length (filter (/='1') str) `f` (length str `div` 2) = '0'
  | otherwise                                         = '1'


---------- PART 2 ----------

getMostCommon :: Int -> Matrix -> Char
getMostCommon index = frequentChar . helper index
  where
    helper :: Int -> Matrix -> String
    helper index []     = []
    helper index (x:xs) = get index x : helper index xs

frequentChar :: String -> Char
frequentChar = head . maximumBy (comparing length) . group . sort

filterIndex :: Int -> Char -> Matrix -> Matrix
filterIndex index char = filter  ( (== char) . get index )

get :: Int -> [a] -> a
get = flip (!!)

getOxygen :: Matrix -> String
getOxygen = help 0 id

getCO2 :: Matrix -> String
getCO2 = help 0 invert

help :: Int -> (Char -> Char) -> Matrix -> String
help i f []  = ""
help i f [x] = x
help i f xs  = help (i+1) f (filterIndex i (f $ getMostCommon i xs) xs)
