{-# LANGUAGE LambdaCase #-}
module Day07.Day07 where

import Data.List
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import Data.Foldable
import Data.Maybe


data CDDest = Root | GoUp | GoDown String
  deriving (Eq, Show, Ord)

data Instr = CD CDDest | File Int
  deriving (Eq, Show, Ord)

parseInstr :: String -> Maybe Instr
parseInstr i = case words i of
  ["$","cd","/"] -> Just $ CD Root
  ["$","cd",".."] -> Just $ CD GoUp
  ["$","cd",d] -> Just $ CD (GoDown d)
  ["$","ls"] -> Nothing
  ["dir", _] -> Nothing
  [n,_] -> Just $ File (read @Int n)
  _ -> Nothing

parse :: String -> [Instr]
parse = mapMaybe  parseInstr . lines


buildSizes :: [Instr] -> M.Map [String] Int
buildSizes = snd . foldl' go ([], M.empty)
  where
    go (currDir, mp) = \case
      CD Root       -> ([], mp)
      CD GoUp       -> (tail currDir, mp)
      CD (GoDown d) -> (d:currDir, mp)
      File sz       -> (currDir, M.unionWith (+) mp $
        M.fromList (map (,sz) (tails currDir)))


part1 :: String -> Int
part1 s = sum $ M.filter (<= 100000) sizes
  where
    instrs = parse s

    sizes = buildSizes instrs

part2 :: String -> Int
part2 s = fromJust . find (\size -> (totalSize - size) <= 70_000_000 - 30_000_000 ) $ sort (toList sizes)
  where
    instrs = parse s
    
    sizes = buildSizes instrs

    totalSize = sizes M.! []

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr "Part 1: "
  print $ part1 input
  putStr "Part 2: "
  print $ part2 input
