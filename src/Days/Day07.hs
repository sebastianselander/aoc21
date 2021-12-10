module Days.Day07
    ( solve1
    , solve2
    ) where

import Misc
import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 = ((sum .) =<< map . (abs .) . (-) . uncurry (!!) . (sort &&& (flip div 2 . length))) . map read . splitOn ','

solve2 :: String -> Int
solve2 = ((sum .) =<< map . ((flip div 2 . uncurry (*) . (id &&& (+1)) . abs) .) . (-) . uncurry div . (sum &&& length)) . map read . splitOn ','
