module Main where

import Text.Printf
import System.Environment (getArgs)
import System.Directory (listDirectory)
import System.CPUTime (getCPUTime)
import Day1 qualified as D1
import Day2 qualified as D2

type Day = String
type Part = String

main :: IO ()
main = do
    args <- getArgs
    putStrLn ""
    case args of
      ["all"] -> printAllSolutions
      [day,part] -> printSolution day part

pickSolver :: (String -> String) -> (String -> String) -> Part -> (String -> String)
pickSolver p1 p2 "1" = p1
pickSolver p1 p2 "2" = p2
pickSolver _ _ _   = error "no such part"


printSolution :: Day -> Part -> IO ()
printSolution day part = do
   input <- readFile $ "input/Day" <> day <> ".txt"
   case day of
     "1" -> evalNTime day part (pickSolver D1.solve1 D1.solve2 part) input
     "2" -> evalNTime day part (pickSolver D2.solve1 D2.solve2 part) input
     -- "3" -> evalNTime day part (pickSolver D3.solve1 D3.solve2 part) input
     -- "4" -> evalNTime day part (pickSolver D4.solve1 D4.solve2 part) input
     -- "5" -> evalNTime day part (pickSolver D5.solve1 D5.solve2 part) input
     -- "6" -> evalNTime day part (pickSolver D6.solve1 D6.solve2 part) input

evalNTime :: Day -> Part -> (String -> String) -> String -> IO ()
evalNTime day part f inp = do
    preTime <- getCPUTime
    putStrLn $ "Day " <> day <> ", part " <> part
    putStrLn $ "Solution: " ++ (f inp)
    postTime <- getCPUTime
    let diff = (fromIntegral (postTime - preTime)) / (10^12)
    printf   "Time:     %0.3f sec\n" (diff :: Double)
    putStrLn ""

printAllSolutions :: IO ()
printAllSolutions = printAllSolutions' 1

printAllSolutions' :: Int -> IO ()
printAllSolutions' n = do
    daysCompleted <-
        length <$> listDirectory "/home/sebastian/Documents/github/aoc21/src"
    if (n == daysCompleted + 1) then
      pure ()
    else do
      printSolution (show n) "1"
      printSolution (show n) "2"
      putStrLn "-------------------"
      printAllSolutions' (n+1)
