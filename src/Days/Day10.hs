{-# LANGUAGE LambdaCase #-}

module Days.Day10
    ( solve1
    , solve2
    ) where

import Misc

solve1 :: String -> Int
solve1 = sum 
       . map (toPoints P1) 
       . filter (/= 'x') 
       . map (corrupted []) 
       . parseInput

solve2 :: String -> Int
solve2 xs = nums !! (length nums `div` 2)
  where
    nums = sort 
         . map (score . (incomplete [])) 
         . filter (not . isCorrupted) 
         . parseInput $ xs

parseInput :: String -> [String]
parseInput = lines

flipBracket :: Char -> Char
flipBracket = \case
    '(' -> ')'
    '[' -> ']'
    '{' -> '}'
    '<' -> '>'
    _   -> error "incorrect char"

match :: Char -> Char -> Bool
match '(' ')' = True
match '[' ']' = True
match '{' '}' = True
match '<' '>' = True
match _   _   = False

isLeft :: Char -> Bool
isLeft = \case 
    '(' -> True
    '[' -> True
    '{' -> True
    '<' -> True
    _   -> False

score :: String -> Int
score = foldl' (\acc x -> (toPoints P2 x) + (acc * 5)) 0

data P = P1 | P2

toPoints :: P -> Char -> Int
toPoints p c = case p of 
    P1 -> case c of
            ')' -> 3
            ']' -> 57
            '}' -> 1197
            '>' -> 25137
            _   -> error "not a bracket"
    P2 -> case c of
            ')' -> 1
            ']' -> 2
            '}' -> 3
            '>' -> 4
            _   -> error "not a bracket"

corrupted :: [Char] -> [Char] -> Char
corrupted _          []        = 'x'
corrupted []         (l:ine) = corrupted [l] ine
corrupted (s:tack) (l:ine) = case isLeft l of
    True -> corrupted (l:s:tack) ine
    False -> if match s l then corrupted tack ine else l

isCorrupted :: String -> Bool
isCorrupted xs = case corrupted [] xs of
    'x' -> False
    _   -> True

incomplete :: [Char] -> [Char] -> [Char]
incomplete stack    []      = map flipBracket stack
incomplete []       (l:ine) = incomplete [l] ine
incomplete (s:tack) (l:ine) = case isLeft l of
    True -> incomplete (l:s:tack) ine
    False -> case match s l of
        True -> incomplete tack ine
        False -> error "lines should not be corruped"
