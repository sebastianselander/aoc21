{-# LANGUAGE LambdaCase #-}

module Days.Day16
    ( solve1
    , solve2
    ) where

import Misc
import Data.List.Split
import Debug.Trace (trace)

solve1 :: String -> Int
solve1 = const 0

solve2 :: String -> Int
solve2 = const 0

d16 :: String
d16 = unsafePerformIO $ readFile "input/Day16.txt"

toBin :: [Char] -> [Char]
toBin = concatMap hexToBin . head . lines
  where
    hexToBin = \case
      '0' -> "0000"; '1' -> "0001"; '2' -> "0010"; '3' -> "0011";
      '4' -> "0100"; '5' -> "0101"; '6' -> "0110"; '7' -> "0111";
      '8' -> "1000"; '9' -> "1001"; 'A' -> "1010"; 'B' -> "1011";
      'C' -> "1100"; 'D' -> "1101"; 'E' -> "1110"; 'F' -> "1111";

toDec :: [Char] -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

data Desc = Version | ID | Operator | Literal | BitLength | PacketCount | Rest
    deriving (Show, Eq)

parse :: [Char] -> [(Desc, [Char])]
parse = parse' Version 0 0 
  where
    parse' :: Desc -> Int -> Int -> [Char] -> [(Desc, [Char])]
    parse' _ _        _     [] = []
    parse' d bitCount instr xs = case d of
        Version -> (Version, take 3 xs) : parse' ID 0 0 (drop 3 xs)
        ID      -> case take 3 xs of
            ['1','0','0'] -> (ID, take 3 xs) : parse' Literal 0 (instr + 5) (drop 3 xs) 
            _             -> (ID, take 3 xs) : parse' Operator 0 0 (drop 3 xs)
        Operator -> case take 1 xs of
            ['0'] -> (Operator, take 1 xs)   : parse' BitLength 0 0 (drop 1 xs)
            ['1'] -> (Operator, take 1 xs)   : parse' PacketCount 0 0 (drop 1 xs)
        BitLength -> (BitLength, take 15 xs) : parse' Version 0 0 (drop 15 xs)
        PacketCount -> (PacketCount, take 11 xs) : parse' Version 0 0 (drop 11 xs)
        Literal -> case take 5 xs of
            ('1' : rest) -> (Literal, rest) : parse' Literal (bitCount + 5) 0 (drop 5 xs)
            ('0' : rest) -> (Literal, rest) : parse' Version 0 0 (drop 5 xs)

sumVersions :: [Char] -> Int
sumVersions = sum . map (toDec . snd) . filter ((==Version) . fst) . parse

toDecList :: [(Desc, [Char])] -> [(Desc, Int)]
toDecList []     = []
toDecList (x:xs) = second toDec x : toDecList xs

-- | Remove outermost layer before
pickSubPackets :: Int ->  [(Desc, Int)] -> [[(Desc, Int)]]
pickSubPackets 0 xs = []
pickSubPackets n xs = let (done, rest) = span ((/=Version) . fst) (drop 1 xs)
                      in  done : pickSubPackets (n - 1) rest


--------------------------------------- PART 2 ------------------------------------------




