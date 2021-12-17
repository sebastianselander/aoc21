module Misc ( 

              module Data.List,
              module Data.Char,
              module Data.Ord,
              module Data.Bool,
              module Control.Arrow,
              module Control.Monad,
              module Data.Function,
              module System.IO.Unsafe,

              splitOn,

            )

            where

import           Data.List
import           Data.Char
import           Data.Ord
import           Data.Bool
import           Control.Arrow
import           Control.Monad
import           Data.Function
import           System.IO.Unsafe

splitOn :: Char -> String -> [String]
splitOn c str = case break (==c) str of
                (a, c:b) -> a : splitOn c b 
                (a, "")    -> [a]
