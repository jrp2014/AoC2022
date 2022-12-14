-- https://github.com/blaz-kranjc/aoc22/blob/main/Day9.hs
module Day09.Day09 where

import qualified Data.Set as Set

parse :: String -> [Char]
parse s = replicate (read . drop 2 $ s) (head s)

headPath :: [Char] -> [(Int, Int)]
headPath = reverse . fst . foldl update ([(0, 0)], (0, 0))
  where
    update (visited, (x, y)) 'R' = ((x + 1, y) : visited, (x + 1, y))
    update (visited, (x, y)) 'L' = ((x - 1, y) : visited, (x - 1, y))
    update (visited, (x, y)) 'U' = ((x, y + 1) : visited, (x, y + 1))
    update (visited, (x, y)) _c = ((x, y - 1) : visited, (x, y - 1))

tailVisited :: [(Int, Int)] -> [(Int, Int)]
tailVisited = reverse . foldl update []
  where
    update [] v = [v]
    update pos@((x', y'):_) (x, y)
      | abs (x - x') <= 1 && abs (y - y') <= 1 = pos
      | otherwise = (x' + signum (x - x'), y' + signum (y - y')) : pos

main :: IO ()
main = do
  input <- readFile "input.txt"
  let headMotion = headPath . concatMap parse . lines $ input
  print . length . Set.fromList . tailVisited $ headMotion
  print . length . Set.fromList . flip (!!) 9 . iterate tailVisited $ headMotion
