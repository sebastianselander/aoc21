module Days.Day14
    ( solve1
    , solve2
    ) where

import Misc
import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 str = ans 
           $ addLast (template $ parseInput str) 
           $ combine 
           $ convertPairs 
           $ combine 
           $ nSteps 10 str

solve2 :: String -> Int
solve2 str = ans 
           $ addLast (template $ parseInput str) 
           $ combine 
           $ convertPairs 
           $ combine 
           $ nSteps 40 str

data Polymer = 
    P { template :: String
      , instructions :: [(String, Char)]
      } deriving Show

parseInput :: String -> Polymer
parseInput str = P { template = temp, instructions = instr }
  where
    temp = head (lines str)
    instr = map (\str -> (take 2 str, str !! 6)) $ drop 2 (lines str)

parseP2 :: Polymer -> [(String, Int)]
parseP2 p = combine (map (\x -> (head x, length x)) 
          $ group 
          $ sort 
          $ pairExists 
          $ template p) 
          ++ foldl' (\acc (str,c) -> (str,0) : acc) [] (instructions p)
  where
    pairExists :: String -> [String]
    pairExists []         = []
    pairExists [a]        = []
    pairExists (a:b:rest) = [a,b] : pairExists (b:rest)


findAndPair :: [(String, Char)] -> String -> (String, String)
findAndPair instr str = match $ head $ filter ((str==) . fst) instr
  where
    match :: (String, Char) -> (String, String)
    match ([a,b],c) = ([a,c],[c,b])

combine :: [(String, Int)] -> [(String, Int)]
combine xs = uncurry zip 
           $ (map (fst . head) &&& map (sum . map snd)) 
           $ groupBy (\(a,b) (c,d) -> a == c) 
           $ sort xs

nSteps :: Int -> String -> [(String, Int)]
nSteps n str = go n (instructions $ parseInput str) (parseP2 $ parseInput str)
  where

    go :: Int -> [(String, Char)] -> [(String, Int)] -> [(String, Int)]
    go 0 instr temp = temp
    go n instr temp = go (n-1) instr (step instr temp)

    step :: [(String, Char)] -> [(String, Int)] -> [(String, Int)]
    step instr []           = []
    step instr ((str,n):xs) =
        let (a,b) = findAndPair instr str
        in combine $ (a,n) : (b,n) : step instr xs

convertPairs :: [(String, Int)] -> [(String, Int)]
convertPairs []               = []
convertPairs (([a,b],n):rest) = (a:"",n) : convertPairs rest

addLast :: String -> [(String, Int)] -> [(String, Int)]
addLast start []         = []
addLast start ((a,b):xs) = if a == (:[]) (last start) then 
                             (a,b+1) : addLast start xs 
                           else 
                             (a,b) : addLast start xs

ans :: [(String, Int)] -> Int
ans = uncurry (-) . (maximum &&& minimum) . map snd
