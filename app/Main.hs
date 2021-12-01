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
      ["all"] -> do
          preTime <- getCPUTime
          printAllSolutions
          postTime <- getCPUTime
          let diff = (fromIntegral (postTime - preTime)) / (10^12)
          putStrLn "---------------------------------"
          printf"\nTotal evalauation time around: %0.3f sec\n" (diff :: Double)
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
     -- "2" -> evalNTime day part (pickSolver D2.solve1 D2.solve2 part) input
     -- "3" -> evalNTime day part (pickSolver D3.solve1 D3.solve2 part) input
     -- "4" -> evalNTime day part (pickSolver D4.solve1 D4.solve2 part) input
     -- "5" -> evalNTime day part (pickSolver D5.solve1 D5.solve2 part) input
     -- "6" -> evalNTime day part (pickSolver D6.solve1 D6.solve2 part) input

evalNTime :: Day -> Part -> (String -> String) -> String -> IO ()
evalNTime day part f inp = do
    preTime <- getCPUTime
    putStrLn   "+---------------------------+"
    putStrLn $ "|       Day " <> day <> " - part " <> part <> "      |"
    putStrLn $ "+---------------------------+"
    putStrLn $ "| Solution: " <> (f inp) <> "            |"
    postTime <- getCPUTime
    let diff = (fromIntegral (postTime - preTime)) / (10^12)
    printf   "| Time: %0.3f sec           |\n" (diff :: Double)
    putStrLn   "+---------------------------+"

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
      printAllSolutions' (n+1)


------ refactoring ------

evalSolution :: (Day,Part) -> IO (String, String)
evalSolution dp = do
    preTime <- getCPUTime
    x <- match dp
    postTime <- getCPUTime
    let diff = show $ (fromIntegral (postTime - preTime)) / (10^12)
    return (x,diff)

evalAll :: IO ([String],String)
evalAll = do
    c <- length <$> listDirectory "/home/sebastian/Documents/github/aoc21/src"
    preTime <- getCPUTime
    results <- sequenceA $ reverse $ go 1
    postTime <- getCPUTime
    let diff = show $ (fromIntegral (postTime - preTime)) / (10^12)
    pure (results,diff)
    where
        go :: Int -> [IO String]
        go 0 = []
        go c = match (show c,"2") : match (show c,"1") :  go (c-1)

match :: (Day,Part) -> IO String
match (day,part) = do
    input <- readFile $ "input/Day" <> day <> ".txt"
    case day of
      "1" -> pure $ (pickSolver D1.solve1 D1.solve2 part) input
      -- "2" -> pickSolver (pickSolver D2.solve1 D2.solve2 part) input


--TODO: pretty printer
-- refactor main with new code

