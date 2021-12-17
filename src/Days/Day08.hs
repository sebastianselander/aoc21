module Days.Day08
    ( solve1
    , solve2
    ) where

import Misc

solve1 :: String -> Int
solve1 = length . filter lengthOk . parseInput1

solve2 :: String -> Int
solve2 = sum . map (intListtoInt . getResult) . parseFile

-----------------------------------funcs---------------------------------------

parseInput1 :: String -> [String]
parseInput1 = concatMap (words . drop 2 . dropWhile (/= '|')) . lines

lengthOk :: [a] -> Bool
lengthOk x = length x == 2 || length x == 3 || length x == 4 || length x == 7

getSegment :: [String] -> String-> Int
getSegment known str
  | length str == 2 = 1
  | length str == 3 = 7
  | length str == 4 = 4
  | length str == 5 = case length (str `intersect` head known) of 
       2 -> 3 
       1 -> case length (str `intersect` (known !! 2)) of
           2 -> 2
           3 -> 5
           _ -> error "fail on len 5 first"
       _ -> error "fail on len 5 second"
  | length str == 6 = case length (str `intersect` head known) of
        1 -> 6
        2 -> case length (str `intersect` (known !! 2)) of
            4 -> 9
            3 -> 0
            _ -> error "fail on len 6 first"
        _ -> error "fail on len 5 second"
  | length str == 7 = 8
  | otherwise       = error "not on a seg display"

getKnown :: [String] -> [String]
getKnown = sortOn length . filter lengthOk 

figureout :: [String] -> [(String, Int)]
figureout str = zip str (map (getSegment (getKnown str)) str)

convert :: [(String, Int)] -> String -> Int
convert []         str = 0
convert ((a,b):xs) str 
  | a == str  = b
  | otherwise = convert xs str

getResult :: ([String], [String]) -> [Int]
getResult (a,b) = map (convert (figureout a)) b

intListtoInt :: [Int] -> Int
intListtoInt [a,b,c,d] = a * 1000 + b * 100 + c * 10 + d

parseFile :: String -> [([String], [String])]
parseFile = map parseRow . lines
  where
    parseRow :: String -> ([String], [String])
    parseRow str = (map sort $ words $ takeWhile (/= '|') str
                   , map sort $ words $ drop 1 $ dropWhile (/= '|') str)
                   --just use spliton if you're not an ape like :)
