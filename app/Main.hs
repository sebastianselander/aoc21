module Main where

import Text.Printf
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import Data.Text qualified as T
import Data.Text
import Day1 qualified as D1

main :: IO ()
main = do
    preTime <- getCPUTime
    putStrLn "\nStarting..."
    [d,p] <- getArgs
    input <- readFile $ "input/Day" <> d
    let day = read d :: Int
    let part = read p :: Int
    case day of
      1 -> print $ pickSolver D1.solve1 D1.solve2 part (T.pack input)
    postTime <- getCPUTime
    -- no shame yanked from wiki
    let diff = (fromIntegral (postTime - preTime)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)





pickSolver :: (Text -> Text) -> (Text -> Text) -> Int -> (Text -> Text)
pickSolver p1 p2 1 = p1
pickSolver p1 p2 2 = p2
pickSolver _ _ _   = error "no such part"
