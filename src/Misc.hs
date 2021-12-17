module Misc ( 

              module Data.List,
              module Data.Char,
              module Data.Ord,

              parseAsStringGenList,
              parseAsGenStringList,
              parseAsGenList, 
              parseAsMatrix,
              linesSeq,
              splitOn,
              unsafePerformIO

            )

            where

import           Data.List
import           Data.Char
import           Data.Ord
import           Data.Text.Internal.Private (span_)
import           Data.Text.Unsafe (unsafeTail)
import           Data.Sequence qualified as Seq
import           Data.Text qualified as T
import           Data.Map qualified as M
import           Relude qualified as R
import           System.IO.Unsafe (unsafePerformIO)


parseAsStringGenList :: Read a => String -> [(String,a)]
parseAsStringGenList = map ((\ (x,y) -> (x, read y)) . toTuple . words) . lines
  where
    toTuple :: [String] -> (String,String)
    toTuple [x,y] = (x,y)
    toTuple _        = error "sadge"

parseAsGenStringList :: Read a => String -> [(a,String)]
parseAsGenStringList = map ((\ (x,y) -> (read x,y)) . toTuple . words) . lines
  where
    toTuple :: [String] -> (String,String)
    toTuple [x,y] = (x,y)
    toTuple _        = error "sadge"

parseAsGenList :: Read a => String -> [a]
parseAsGenList = map read . words

parseAsMatrix :: String -> [String]
parseAsMatrix = lines

splitOn :: Char -> String -> [String]
splitOn c str = case break (==c) str of
                (a, c:b) -> a : splitOn c b 
                (a, "")    -> [a]

linesSeq :: R.Text -> R.Seq R.Text
linesSeq ps
  | T.null ps = Seq.Empty
  | otherwise = h Seq.:<| rest
  where
    rest :: R.Seq R.Text
    rest = if T.null t then Seq.Empty else linesSeq (unsafeTail t)

    (h, t) = T.span (/= '\n') ps
{-# INLINE linesSeq #-}
