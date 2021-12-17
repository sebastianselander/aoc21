module Days.Day13
    ( solve1
    , solve2
    ) where

import Misc
import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 = length 
       . sort 
       . nub 
       . foldEmUp 
       . uncurry (I) 
       . (grid &&& (:[]) . head . folds) 
       . parseInput

solve2 :: String -> Int
solve2 = const 0

data Instruction = 
    I
    { grid :: [(Int, Int)] 
    , folds :: [(Char,Int)]
    } deriving Show

parseInput :: String -> Instruction
parseInput str = I { grid = first, folds = second}
  where
    first = map ((\[a,b] -> (read a,read b)) . (splitOn ',')) 
          $ takeWhile (not . null) (lines str)
    second = map (\str -> (head str, read (drop 2 str))) 
           $ map (drop 11) 
           $ tail 
           $ dropWhile (not . null) (lines str)

horFold :: Int -> [(Int, Int)] -> [(Int, Int)]
horFold instr = map folder
  where
    folder (x,y) = if y > instr then (x,2*instr-y) else (x,y)

verFold :: Int -> [(Int, Int)] -> [(Int, Int)]
verFold instr = map folder
  where
    folder (x,y) = if x > instr then (2*instr-x,y) else (x,y)

foldEmUp :: Instruction -> [(Int, Int)]
foldEmUp (I grid []) = grid
foldEmUp (I grid (f:olds)) = case f of
    ('x',y) -> foldEmUp (I (verFold y grid) olds)
    ('y',x) -> foldEmUp (I (horFold x grid) olds)

change :: (Int, Int) -> [String] -> [String]
change (x,y) (g:gs)
  | y == 0 = replace x g '#' : gs
  | otherwise = g : change (x,y-1) gs

replace :: Int -> String -> Char -> String
replace x l c = let (a,b) = splitAt x l
                in a ++ (c:b)

pp :: [(Int, Int)] -> [String] -> IO ()
pp []     canvas = mapM_ putStrLn canvas
pp (x:xs) canvas = pp xs (change x canvas)

blank :: [String]
blank = replicate 6 (replicate 25 ' ')

run :: String -> IO ()
run str = pp (foldEmUp $ parseInput str) blank
