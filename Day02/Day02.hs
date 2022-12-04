module Day02.Day02 where

data A where
  AA :: A
  AB :: A
  AC :: A
  deriving (Show, Read)

data B where
  BX :: B
  BY :: B
  BZ :: B
  deriving (Show, Read)

parse :: String -> [(A, B)]
parse s = map parseLine ls
  where
    ls = lines s

    parseLine :: String -> (A, B)
    parseLine l = (\[a, b] -> (read @A ('A' : a), read @B ('B' : b))) $ words l

shapeScore :: B -> Int
shapeScore BX = 1
shapeScore BY = 2
shapeScore BZ = 3

outcome :: A -> B -> Int
outcome AA BX = 3
outcome AA BY = 6
outcome AA BZ = 0
outcome AB BX = 0
outcome AB BY = 3
outcome AB BZ = 6
outcome AC BX = 6
outcome AC BY = 0
outcome AC BZ = 3

desiredOutcome :: B -> Int
desiredOutcome BX = 0
desiredOutcome BY = 3
desiredOutcome BZ = 6

pick :: A -> B -> B
pick a b = head [x | x <- [BX, BY, BZ], outcome a x == desiredOutcome b]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let pinput = parse input

  putStr "Part 1: "
  print $ sum [outcome a b + shapeScore b | (a, b) <- pinput]

  putStr "Part 2: "
  print $ sum [outcome a b + shapeScore b | (a, b') <- pinput, let b = pick a b']
