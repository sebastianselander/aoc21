module Misc ( parseAsStringGenList
            , parseAsGenStringList
            , parseAsGenList
            , parseAsMatrix)
            where

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
