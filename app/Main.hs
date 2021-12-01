module Main where

import Text.Printf
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import Day1 qualified as D1

completedDays :: Int
completedDays = 1

main :: IO ()
main = do
    args <- getArgs
    putStrLn ""
    case args of
      ["all"] -> printAllSolutions 1
      [day,part] -> printSolution day part


pickSolver :: (String -> String) -> (String -> String) -> String -> (String -> String)
pickSolver p1 p2 "1" = p1
pickSolver p1 p2 "2" = p2
pickSolver _ _ _   = error "no such part"


printSolution :: String -> String -> IO ()
printSolution day part = do
   input <- readFile $ "input/Day" <> day
   case day of
     "1" -> evalNTime day part (pickSolver D1.solve1 D1.solve2 part) input

evalNTime :: String -> String -> (String -> String) -> String -> IO ()
evalNTime day part f inp = do
    preTime <- getCPUTime
    putStrLn $ "Day " <> day <> ", part " <> part
    putStrLn $ "Solution: " ++ (f inp)
    postTime <- getCPUTime
    let diff = (fromIntegral (postTime - preTime)) / (10^12)
    printf   "Time:     %0.3f sec\n" (diff :: Double)
    putStrLn ""

printAllSolutions :: Int -> IO ()
printAllSolutions n
  | n == completedDays + 1 = pure ()
  | otherwise = do
      printSolution (show n) "1"
      printSolution (show n) "2"
      putStrLn "-------------------"
      printAllSolutions (n+1)


