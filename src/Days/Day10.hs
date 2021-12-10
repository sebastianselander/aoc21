{-# LANGUAGE LambdaCase #-}

module Days.Day10
    ( solve1
    , solve2
    ) where

import Misc
import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 = sum 
       . map toPoints
       . filter (/= 'x') 
       . map (corrupted []) 
       . parseInput

solve2 :: String -> Int
solve2 = uncurry (!!) 
       . (sort &&& (flip div 2 . length)) 
       . map (score . (incomplete [])) 
       . filter (not . isCorrupted) 
       . parseInput

parseInput :: String -> [String]
parseInput = lines

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
score = foldl' (\acc x -> (toPoints x) + (acc * 5)) 0

toPoints :: Char -> Int
toPoints = \case
      '(' -> 1; ')' -> 3
      '[' -> 2; ']' -> 57
      '{' -> 3; '}' -> 1197
      '<' -> 4; '>' -> 25137
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
incomplete stack    []      = stack
incomplete []       (l:ine) = incomplete [l] ine
incomplete (s:tack) (l:ine) = case isLeft l of
    True -> incomplete (l:s:tack) ine
    False -> case match s l of
        True -> incomplete tack ine
        False -> error "lines should not be corruped"
