module Days.Day2
    ( solve1
    , solve2
    ) where

--"forward 5\nup 5"
test :: FilePath -> IO [(String,Int)]
test xs = parseInput <$> readFile xs

parseInput :: String -> [(String,Int)]
parseInput = (map (\(x,y) -> (x,read y))) . (map toTuple) . (map words) . lines
  where
    toTuple :: [String] -> (String,String)
    toTuple (x:y:[]) = (x,y)
    toTuple _        = error "sadge"

solve1 :: String -> Int
solve1 = path (0,0) . parseInput

solve2 :: String -> Int
solve2 = path2 0 (0,0) . parseInput

path :: (Int,Int) -> [(String,Int)] -> Int
path (x,y) []         = x*y
path (x,y) ((inst,n):ab) = case inst of
                      "forward" -> path (x+n,y) ab
                      "up"      -> path (x,y-n) ab
                      "down"    -> path (x,y+n) ab


path2 :: Int -> (Int,Int) -> [(String,Int)] -> Int
path2 aim (x,y) []         = x * y
path2 aim (x,y) ((inst,n):ab) =
  case inst of
    "forward" -> path2 aim     (x + n, y + (n * aim) ) ab
    "up"      -> path2 (aim - n) (x,y) ab
    "down"    -> path2 (aim + n) (x,y) ab





