module Main where

import Text.Printf
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import Day1 qualified as D1

main :: IO ()
main = do
    putStrLn "\nStarting...\n"
    preTime <- getCPUTime
    [day,part] <- getArgs
    input <- readFile $ "input/Day" <> day
    printSolution (read day) (read part) input
    postTime <- getCPUTime
    -- no shame yanked from wiki
    let diff = (fromIntegral (postTime - preTime)) / (10^12)
    printf   "Time:     %0.3f sec\n" (diff :: Double)
    putStrLn "\nDone."


pickSolver :: (String -> String) -> (String -> String) -> Int -> (String -> String)
pickSolver p1 p2 1 = p1
pickSolver p1 p2 2 = p2
pickSolver _ _ _   = error "no such part"

printSolution :: Int -> Int -> String -> IO ()
printSolution day part input = case day of
    1 -> putStrLn $ "Solution: " ++ pickSolver D1.solve1 D1.solve2 part input
    -- 2 -> putStrLn $ "Solution: " ++ pickSolver D2.solve1 D2.solve2 part input
    -- 3 -> putStrLn $ "Solution: " ++ pickSolver D3.solve1 D3.solve2 part input
    -- 4 -> putStrLn $ "Solution: " ++ pickSolver D4.solve1 D4.solve2 part input
