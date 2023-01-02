module Day06.Day06 where

import qualified Data.Set as S
import Data.List ( tails, transpose )



solve :: Int -> String -> Int
solve n xs = length (takeWhile ((< n) . S.size . S.fromList) chunks) + n
  where
    chunks :: [String]
    chunks = transpose . take n $ tails xs



main :: IO ()
main = do
  input <- readFile "input.txt"

  putStr "Part 1: "
  print $ solve 4 input

  putStr "Part 2: "
  print $ solve 14 input
