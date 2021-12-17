module Days.Day17
    ( solve1
    , solve2
    ) where

import Misc
import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 = flip div 2 . uncurry (*) . (id &&& subtract 1) . abs . yMin . parseInput

solve2 :: String -> Int
solve2 = uncurry (+) . (obv &&& brute) . parseInput
  where
    obv (T ax bx ay by) = (bx - ax + 1) * ((abs ay - abs by) + 1)

data Target = T Int Int Int Int

yMin :: Target -> Int
yMin (T _ _ y _) = y

parseInput :: String -> Target
parseInput str = T (from $ x str) (to $ x str) (from $ y str) (to $ y str)
  where
    x    = drop 2 . takeWhile (/=',') . dropWhile (/='x')
    y    = init . drop 2 . dropWhile (/='y')
    from = read . takeWhile (/='.') 
    to   = read . drop 2 . dropWhile (/='.') 

fire :: Target -> Int -> Int -> [(Int, Int)]
fire (T _ _ ay _) dx dy = takeWhile ((>=ay) . snd) $ fire' (0,0) dx dy
  where
    fire' :: (Int, Int) -> Int -> Int -> [(Int, Int)]
    fire' (x,y) dx dy 
      | dx > 0    = (x+dx,y+dy) : fire' (x+dx, y+dy) (dx-1) (dy-1)
      | dx < 0    = (x+dx,y+dy) : fire' (x+dx, y+dy) (dx+1) (dy-1)
      | otherwise = (x+dx,y+dy) : fire' (x+dx, y+dy)  dx    (dy-1)

isIn :: Target -> [(Int, Int)] -> Bool
isIn t = any $ pointIsIn t 
  where
    pointIsIn :: Target -> (Int, Int) -> Bool
    pointIsIn (T ax bx ay by) (x,y) = x >= ax 
                                   && x <= bx 
                                   && y >= ay 
                                   && y <= by

findPossibleRange :: Int -> Target -> [Int]
findPossibleRange n t@(T ax bx _ _) 
  | 2 * n - 1 > bx = []
  | n * (n + 1) `div` 2 < ax = findPossibleRange (n + 1) t
  | otherwise                = n : findPossibleRange (n + 1) t

brute :: Target -> Int
brute t = sum $ map (go t) (findPossibleRange 0 t)
  where
    go :: Target -> Int -> Int
    go t@(T _ _ ay _) x 
      = sum $ map (fromEnum . isIn t . fire t x) [ay .. abs ay - 1]
