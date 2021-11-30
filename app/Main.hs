module Main where

import Relude
import System.Environment (getArgs)
import Day1 qualified as D1
-- import Day2 qualified as D2
-- import Day3 qualified as D3
-- import Day4 qualified as D4
-- import Day5 qualified as D5
-- import Day6 qualified as D6
-- import Day7 qualified as D7
-- import Day8 qualified as D8
-- import Day9 qualified as D9
-- import Day10 qualified as D10
-- import Day11 qualified as D11
-- import Day12 qualified as D12
-- import Day13 qualified as D13
-- import Day14 qualified as D14
-- import Day15 qualified as D15
-- import Day16 qualified as D16
-- import Day17 qualified as D17
-- import Day18 qualified as D18
-- import Day19 qualified as D19
-- import Day20 qualified as D20
-- import Day21 qualified as D21
-- import Day22 qualified as D22
-- import Day23 qualified as D23
-- import Day24 qualified as D24
-- import Day25 qualified as D25

main :: IO ()
main = do
    [d,p,inp] <- getArgs
    case d p of
      1 1  -> Day1.solve1 inp
      1 2  -> Day1.solve2 inp
      -- 2 1  -> Day2.solve1 inp
      -- 2 2  -> Day2.solve2 inp
      -- 3 1  -> Day3.solve1 inp
      -- 3 2  -> Day3.solve2 inp
      -- 4 1  -> Day4.solve1 inp
      -- 4 2  -> Day4.solve2 inp
      -- 5 1  -> Day5.solve1 inp
      -- 5 2  -> Day5.solve2 inp
      -- 6 1  -> Day6.solve1 inp
      -- 6 2  -> Day6.solve2 inp
      -- 7 1  -> Day7.solve1 inp
      -- 7 2  -> Day7.solve2 inp
      -- 8 1  -> Day8.solve1 inp
      -- 8 2  -> Day8.solve2 inp
      -- 9 1  -> Day9.solve1 inp
      -- 9 2  -> Day9.solve2 inp
      -- 10 1 -> Day10.solve1 inp
      -- 10 2 -> Day10.solve2 inp
      -- 11 1 -> Day11.solve1 inp
      -- 11 2 -> Day11.solve2 inp
      -- 12 1 -> Day12.solve1 inp
      -- 12 2 -> Day12.solve2 inp
      -- 13 1 -> Day13.solve1 inp
      -- 13 2 -> Day13.solve2 inp
      -- 14 1 -> Day14.solve1 inp
      -- 14 2 -> Day14.solve2 inp
      -- 15 1 -> Day15.solve1 inp
      -- 15 2 -> Day15.solve2 inp
      -- 16 1 -> Day16.solve1 inp
      -- 16 2 -> Day16.solve2 inp
      -- 17 1 -> Day17.solve1 inp
      -- 17 2 -> Day17.solve2 inp
      -- 18 1 -> Day18.solve1 inp
      -- 18 2 -> Day18.solve2 inp
      -- 19 1 -> Day19.solve1 inp
      -- 19 2 -> Day19.solve2 inp
      -- 20 1 -> Day20.solve1 inp
      -- 20 2 -> Day20.solve2 inp
      -- 21 1 -> Day21.solve1 inp
      -- 21 2 -> Day21.solve2 inp
      -- 22 1 -> Day22.solve1 inp
      -- 22 2 -> Day22.solve2 inp
      -- 23 1 -> Day23.solve1 inp
      -- 23 2 -> Day23.solve2 inp
      -- 24 1 -> Day24.solve1 inp
      -- 24 2 -> Day24.solve2 inp
      -- 25 1 -> Day25.solve1 inp
      -- 25 2 -> Day25.solve2 inp
