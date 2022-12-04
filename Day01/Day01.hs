module Day01.Day01 where

import Data.List.Split
import Data.List

main :: IO()
main = do
  input <- readFile "input.txt"

  let elveCalories =  reverse . sort . map  (sum .  map (read @Int)  . words) $  splitOn "\n\n" input

  putStr "Part 1: "
  print $ head elveCalories

  putStr "Part 2: "
  print $ sum $ take 3 elveCalories


