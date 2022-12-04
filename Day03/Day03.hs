module Day03.Day03 where

import Data.Char
import Data.List
import Data.List.Split

parse :: String -> [([Char], [Char])]
parse s = map parseLine ls
  where
    ls = lines s

    parseLine :: String -> ([Char], [Char])
    parseLine l = splitAt (length l `div` 2) l

priority :: Char -> Int
priority item
  | isLower item = ord item - ord 'a' + 1
  | otherwise = ord item - ord 'A' + 27

part1 :: [([Char], [Char])] -> Int
part1 rs = sum $ map (head . map priority . inBoth) rs
  where
    inBoth :: ([Char], [Char]) -> [Char]
    inBoth (c1, c2) = c1 `intersect` c2

part2 :: [[Char]] -> Int
part2 rs = sum $ map (head . map priority . inAll) cs
  where
    cs = chunksOf 3 rs

    inAll :: [[Char]] -> [Char]
    inAll = foldl1 intersect

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = parse input

  putStr "Part 1: "
  print $ part1 pinput

  putStr "Part 2: "
  print $ part2 (lines input)
