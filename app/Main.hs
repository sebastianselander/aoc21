module Main where

import Text.Printf
import System.Time.Extra            (sleep)
import System.Environment           (getArgs)
import System.Directory             (listDirectory)
import System.CPUTime               (getCPUTime)
import Control.Monad                (replicateM_)


import Days.Day01 qualified as D1
import Days.Day02 qualified as D2
import Days.Day03 qualified as D3
import Days.Day04 qualified as D4
import Days.Day05 qualified as D5
import Days.Day06 qualified as D6
import Days.Day07 qualified as D7
import Days.Day08 qualified as D8
import Days.Day09 qualified as D9
import Days.Day10 qualified as D10
import Days.Day11 qualified as D11
import Days.Day12 qualified as D12
import Days.Day13 qualified as D13
import Days.Day14 qualified as D14
import Days.Day15 qualified as D15
import Days.Day16 qualified as D16
import Days.Day17 qualified as D17
import Days.Day18 qualified as D18
import Days.Day19 qualified as D19
import Days.Day20 qualified as D20
import Days.Day21 qualified as D21
import Days.Day22 qualified as D22
import Days.Day23 qualified as D23
import Days.Day24 qualified as D24
import Days.Day25 qualified as D25

type Day = String
type Part = String
type DPST = ((Day, Part), String, Double)

completedDays :: Int
completedDays = 9

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> do
          pp evalAll
      [day,part] -> pp $ sequenceA [evalSolution (day,part)]
  where
    newLines = flip replicateM_ (putStrLn "")

match :: (Day,Part) -> IO Int
match (day,part) = do
    input <- readFile $ "input/Day" <> day <> ".txt"
    case day of
      "1" ->  pure $ pickSolver D1.solve1 D1.solve2 part   input
      "2" ->  pure $ pickSolver D2.solve1 D2.solve2 part   input
      "3" ->  pure $ pickSolver D3.solve1 D3.solve2 part   input
      "4" ->  pure $ pickSolver D4.solve1 D4.solve2 part   input
      "5" ->  pure $ pickSolver D5.solve1 D5.solve2 part   input
      "6" ->  pure $ pickSolver D6.solve1 D6.solve2 part   input
      "7" ->  pure $ pickSolver D7.solve1 D7.solve2 part   input
      "8" ->  pure $ pickSolver D8.solve1 D8.solve2 part   input
      "9" ->  pure $ pickSolver D9.solve1 D9.solve2 part   input
      "10" -> pure $ pickSolver D10.solve1 D10.solve2 part input
      "11" -> pure $ pickSolver D11.solve1 D11.solve2 part input
      "12" -> pure $ pickSolver D12.solve1 D12.solve2 part input
      "13" -> pure $ pickSolver D13.solve1 D13.solve2 part input
      "14" -> pure $ pickSolver D14.solve1 D14.solve2 part input
      "15" -> pure $ pickSolver D15.solve1 D15.solve2 part input
      "16" -> pure $ pickSolver D16.solve1 D16.solve2 part input
      "17" -> pure $ pickSolver D17.solve1 D17.solve2 part input
      "18" -> pure $ pickSolver D18.solve1 D18.solve2 part input
      "19" -> pure $ pickSolver D19.solve1 D19.solve2 part input
      "20" -> pure $ pickSolver D20.solve1 D20.solve2 part input
      "21" -> pure $ pickSolver D21.solve1 D21.solve2 part input
      "22" -> pure $ pickSolver D22.solve1 D22.solve2 part input
      "23" -> pure $ pickSolver D23.solve1 D23.solve2 part input
      "24" -> pure $ pickSolver D24.solve1 D24.solve2 part input
      "25" -> pure $ pickSolver D25.solve1 D25.solve2 part input

pickSolver :: (String -> Int) -> (String -> Int) -> Part -> (String -> Int)
pickSolver p1 p2 "1" = p1
pickSolver p1 p2 "2" = p2
pickSolver _ _ _   = error "no such part"

evalSolution :: (Day,Part) -> IO DPST
evalSolution (d,p) = do
    preTime <- getCPUTime
    x <- show <$> match (d,p)
    putStr $ (\a -> if not (null a) then "" else a) x -- fuck laziness :)
    postTime <- getCPUTime
    let diff = fromIntegral (postTime - preTime) / (10^12)
    return ((d, p), x, diff)

evalAll :: IO [DPST]
evalAll = do
    sequenceA $ reverse $ go completedDays
    where
        go :: Int -> [IO DPST]
        go 0 = []
        go c = evalSolution (show c,"2") : evalSolution (show c,"1") :  go (c-1)


ppH :: Int -> Int -> Int -> [DPST] -> IO ()
ppH _   _    size [] = putStrLn $ "+" <> replicate size  '-' <> "+" <> replicate (size - 1) '-' <> "+"
ppH day part size (((d, p), sol, time):xs) = do
      putStrLn $ "+" <> replicate size '-' <> "+" <> replicate (size - 1) '-' <> "+"
      putStrLn $ "| Day " <> d <> " - Part " <> p <> replicate (size - dayd day) ' ' <> " | Time:" <> replicate (size - 7) ' ' <> "|"
      putStr $ "| " <> sol <> replicate (size - length sol - 2) ' ' <> " | "
      putStr timeF
      putStr $ replicate (size - timeL - 2) ' ' <> "|\n"
      ppH (day+(part-1)) ((part `mod` 2) + 1) size xs
  where
    timeF = printf "%0.3f s" time :: String
    timeL = length timeF
    dayd d | d >= 10   = 17
           | otherwise = 16

pp :: IO [DPST] -> IO ()
pp x = do
    b <- x
    let sols = map mid b
    let actualSize = max (maximum $ map length sols) 18
    ppH 1 1 actualSize b
  where
    mid (x,y,z) = y
