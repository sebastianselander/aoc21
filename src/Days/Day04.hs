module Days.Day04 (
    solve1,
    solve2
    ) where

import Misc
import Data.Either

solve1 :: String -> Int
solve1 = score . head . uncurry (go []) . parseInput

solve2 :: String -> Int
solve2 = score . last . uncurry (go []) . parseInput


type Slot a      = Either a a
type Instruction = Int
type Board a     = [[Slot a]]

-- Parsing

toRows :: [String] -> Board Int
toRows []               = []
toRows (a:b:c:d:e:rest) = map (Left . read) [a,b,c,d,e] : toRows rest
toRows _                = error "failed converting to rows"

rowsToBoards :: Board Int -> [Board Int]
rowsToBoards []               = []
rowsToBoards (a:b:c:d:e:rest) = [a,b,c,d,e] : rowsToBoards rest
rowsToBoards _                = error "failed converting to boards"

parseInput :: String -> ([Int], [Board Int])
parseInput str = (map read . splitOn ',' . head $ x
                 , rowsToBoards . toRows . concat $ xs)
  where
    (x:xs) = filter (not . null) . map words . parseAsMatrix $ str

-- Helpers

mark :: Int -> Slot Int -> Slot Int
mark y (Left x) | x == y = Right x
mark _ e                 = e

fromEither :: Either a a -> a
fromEither (Left x)  = x
fromEither (Right x) = x

complete :: Board a -> Bool
complete b = row b || col b
  where
    row = any (all isRight)
    col = row . transpose

applyAll :: (a -> b) -> [[[a]]] -> [[[b]]]
applyAll f = map (map (map f))

applyMark :: Int -> [Board Int] -> [Board Int]
applyMark = applyAll . mark

score :: (Int, Board Int) -> Int
score (instr, board) = (*) instr . sum . lefts . concat $ board

-- Part 1 & 2, only difference is `head` for part 1 and `last` for part 2

step :: Int -> [Board Int] -> [Board Int]
step instr board = filter complete $ applyMark instr board

go :: [(Int,Board Int)] -> [Int] -> [Board Int] -> [(Int,Board Int)]
go buffer []           board = reverse buffer
go buffer (first:rest) board = case step first board of
    [] -> go buffer rest (applyMark first board)
    xs -> go (putIn xs first buffer) rest (applyMark first board)
     where
       putIn :: [Board Int] -> Int -> [(Int, Board Int)] -> [(Int, Board Int)]
       putIn []     n buffer = buffer
       putIn (x:xs) n buffer = 
           if strippedBoard x `elem` strippedBuffer buffer then
              putIn xs n buffer
           else 
              putIn xs n ((n,x):buffer) 

       strippedBoard :: Board Int -> [[Int]]
       strippedBoard = map (map fromEither)
       strippedBuffer :: [(Int, Board Int )] -> [[[Int]]]
       strippedBuffer = map (map (map fromEither) . snd)

