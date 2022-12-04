module Day04.Day04 where

import Data.List.Split

parse :: String -> [((Int, Int), (Int, Int))]
parse s = map parseLine ls
  where
    ls = lines s

    parseLine :: String -> ((Int, Int), (Int, Int))
    parseLine l = (parseRange x, parseRange y)
      where
        [x, y] = splitOn "," l

    parseRange :: String -> (Int, Int)
    parseRange r = (read l, read u)
      where
        [l, u] = splitOn "-" r

between :: Ord a => (a, a) -> a -> Bool
between (a, b) c = a <= c && c <= b

contains :: ((Int, Int), (Int, Int)) -> Bool
contains ((a, b), (c, d)) = between (a, b) c && between (a, b) d || between (c, d) a && between (c, d) b

overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ((a, b), (c, d)) = between (a, b) c || between (a, b) d || between (c, d) a || between (c, d) b

countBy :: (a -> Bool) -> [a] -> Int
countBy p xs = length $ filter p xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = parse input

  putStr "Part 1: "
  print $ countBy contains pinput

  putStr "Part 2: "
  print $ countBy overlaps pinput
