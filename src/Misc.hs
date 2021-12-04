module Misc ( 

              module Data.List,
              module Data.Char,
              module Data.Ord,

              parseAsStringGenList,
              parseAsGenStringList,
              parseAsGenList, 
              parseAsMatrix,
              linesSeq

            )

            where

import           Data.List
import           Data.Char
import           Data.Ord
import           Data.Text.Internal.Private (span_)
import           Data.Text.Unsafe (unsafeTail)
import           Data.Sequence qualified as Seq
import           Data.Text qualified as T
import           Relude qualified as R


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


linesSeq :: R.Text -> R.Seq R.Text
linesSeq ps
  | T.null ps = Seq.Empty
  | otherwise = h Seq.:<| rest
  where
    rest :: R.Seq R.Text
    rest = if T.null t then Seq.Empty else linesSeq (unsafeTail t)

    (h, t) = T.span (/= '\n') ps
{-# INLINE linesSeq #-}
