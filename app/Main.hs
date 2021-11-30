module Main where

import System.Environment (getArgs)
import Data.Text qualified as T
import Data.Text
import Day1 qualified as D1

main :: IO ()
main = do
    [d,p] <- getArgs
    input <- readFile $ "input/Day" <> d
    let day = read d :: Int
    let part = read p :: Int
    case day of
      1 -> print $ pickSolver D1.solve1 D1.solve2 part (T.pack input)

pickSolver :: (Text -> Text) -> (Text -> Text) -> Int -> (Text -> Text)
pickSolver p1 p2 1 = p1
pickSolver p1 p2 2 = p2
pickSolver _ _ _   = error "no such part"
