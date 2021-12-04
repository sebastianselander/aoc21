module Days.Day04
    ( solve1
    , solve2
    ) where

import Misc
import Data.Either

solve1 :: String -> Int
solve1 str = sumBoard $ head $ go [] instr board
  where
   (instr, board) = parseInput str

solve2 :: String -> Int
solve2 str = sumBoard $ last $ go [] instr board
  where
    (instr, board) = parseInput str

parseInput :: String -> ([Int], [Board Int])
parseInput str = (map read $ splitOn ',' $ head x, rowsToBoards $ toRows $ concat xs)
  where
    (x:xs) = filter (not . null) $ map words $ parseAsMatrix str

splitOn :: Char -> String -> [String]
splitOn c str = case break (==c) str of
                (a, c:b) -> a : splitOn c b 
                (a, "")    -> [a]

type Slot a      = Either a a
type Instruction = Int
type Board a      = [[Slot a]]


-- Helpers

toRows :: [String] -> Board Int
toRows []               = []
toRows (a:b:c:d:e:rest) = map (Left . read) [a,b,c,d,e] : toRows rest
toRows _                = error "failed converting to rows"

rowsToBoards :: Board Int -> [Board Int]
rowsToBoards []               = []
rowsToBoards (a:b:c:d:e:rest) = [a,b,c,d,e] : rowsToBoards rest
rowsToBoards _                = error "failed converting to boards"

mark :: Int -> Slot Int -> Slot Int
mark y (Left x) | x == y = Right x
mark _ e                 = e

mapB :: (Slot a -> Slot a) -> Board a -> Board a
mapB f board = map (map f) board

fromEither :: Slot a -> a
fromEither (Left x) = x
fromEither (Right x) = x

complete :: Board a -> Bool
complete b = row b || col b
  where
    row = or . map (and . (map isRight))
    col = or . map (and . (map isRight)) . transpose

applyAll :: (Slot a -> Slot a) -> [Board a] -> [Board a]
applyAll _ []     = []
applyAll f (x:xs) = mapB f x : applyAll f xs

applyMark :: Int -> [Board Int] -> [Board Int]
applyMark n = applyAll (mark n)

sumBoard :: (Int, Board Int) -> Int
sumBoard (instr, board) = (*) instr . sum . lefts . concat $ board

-- Part 1 & 2, only difference is `head` for part 1 and `last` for part 2

oneStep :: Int -> [Board Int] -> [Board Int]
oneStep instr board = filter complete (applyMark instr board)

go :: [(Int,Board Int)] -> [Int] -> [Board Int] -> [(Int,Board Int)]
go buffer []           board = reverse buffer
go buffer (first:rest) board = case (oneStep first board) of
    [] -> go buffer rest (applyMark first board)
    xs -> go (putIn xs first buffer) rest (applyMark first board)

     where

       putIn :: [Board Int] -> Int -> [(Int, Board Int)] -> [(Int, Board Int)]
       putIn []     n buffer = buffer
       putIn (x:xs) n buffer = case (strippedBoard x `elem` strippedBuffer buffer) of
           True -> putIn xs n buffer
           False -> putIn xs n ((n,x):buffer) 

       strippedBoard :: Board Int -> [[Int]]
       strippedBoard = map (map fromEither)
       strippedBuffer :: [(Int, Board Int )] -> [[[Int]]]
       strippedBuffer = map (map (map fromEither)) . map snd 

