-- Writeup at https://work.njae.me.uk/2022/12/08/advent-of-code-2022-day-8/

module Day08.Day08 where

-- import Data.Char
import Data.List


data Tree = Tree Int Bool -- height, isVisible
  deriving (Show, Eq)
type Forest = [[Tree]]

main :: IO ()
main = 
  do  
      text <- readFile "input.txt"
      let forest = fmap (fmap readTree) $ lines text
      print $ part1 forest
      print $ part2 forest

part1, part2 :: Forest -> Int
part1 = countVisible . setVisibilityForest

part2 forest = maximum scores
  where nrows = length forest
        ncols = length $ head forest
        scores = [scenicScore forest r c | r <- [0 .. (nrows - 1)], c <- [0 .. (ncols - 1)]]


readTree :: Char -> Tree
readTree h = Tree (read [h]) False

isVisible :: Tree -> Bool
isVisible (Tree _ v) = v

treeHeight :: Tree -> Int
treeHeight (Tree h _) = h


setVisibility :: [Tree] -> [Tree]
setVisibility row = reverse $ snd $ foldl' vis (-1, []) row
  where vis (highest, tagged) (Tree height visible)
          | height > highest = (height, (Tree height True) : tagged)
          | otherwise = (highest, (Tree height visible) : tagged)

setVisibilityOrient :: Forest -> Forest
setVisibilityOrient = fmap setVisibility

setVisibilityForest :: Forest -> Forest
setVisibilityForest forest = (!!4) $ iterate f forest
  where f = rotate . setVisibilityOrient
        rotate = (fmap reverse) . transpose 

countVisible :: Forest -> Int
countVisible forest = length $ filter isVisible $ concat forest



viewDistance :: Int -> [Tree] -> Int
viewDistance h trees = length $ takeWhile1 (< h) $ fmap treeHeight trees

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 f (x:xs) 
  | f x == True = x : (takeWhile1 f xs)
  | otherwise = [x]

tracks :: Forest -> Int -> Int -> [[Tree]]
tracks forest row col = [reverse l, drop 1 r, reverse u, drop 1 d]
  where (l, r) = splitAt col (forest !! row)
        (u, d) = splitAt row ((transpose forest) !! col)

scenicScore :: Forest -> Int -> Int -> Int
scenicScore forest row col = foldl' (*) 1 $ fmap (viewDistance h) directions
  where directions = tracks forest row col
        h = treeHeight $ (forest!!row)!!col

