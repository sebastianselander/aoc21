module Days.Day1
    ( solve1
    , solve2
    ) where
import Misc (parseAsGenList)
import Data.List (foldl')


solve1 :: String -> Int
solve1 = depth1 . parseAsGenList

solve2 :: String -> Int
solve2 = depth2 . parseAsGenList

--
-- Funner :)
--

depth1 :: [Int] -> Int
depth1 xs = sum $ zipWith (curry (fromEnum . uncurry (>))) (tail xs) xs

depth2 :: [Int] -> Int
depth2 xs = depth1 $ map (uncurry3 (+)) $ zip3 (tail (tail xs)) (tail xs) xs
  where
      uncurry3 f (x,y,z) = x `f` y `f` z

--
-- Clear and easy solutions
--

recursive :: [Int] -> Int
recursive (x:y:xs)
  | x < y     = 1 + recursive (y:xs)
  | otherwise =     recursive (y:xs)
recursive _      = 0

recursive2 :: Int -> [Int] -> Int
recursive2 n (x:y:z:rest)
  | n < (x+y+z) = 1 + recursive2 (x+y+z) (y:z:rest)
  | otherwise   =     recursive2 (x+y+z) (y:z:rest)
recursive2 _ _      = 0
