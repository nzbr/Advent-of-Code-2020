module Main where

import Data.List

chalf :: Int -> Int
chalf a
  | even a = a `div` 2
  | otherwise = (a `div` 2) + 1

getSeat :: (Int, Int) -> [Char] -> Int
getSeat (min, _) "L" = min
getSeat (_, max) "R" = max
getSeat (min, max) ('L' : xs) = getSeat (min, max - chalf (max - min)) xs
getSeat (min, max) ('R' : xs) = getSeat (min + chalf (max - min), max) xs

seatNr :: (Int, Int) -> [Char] -> Int
seatNr (_, max) ('B' : x : xs) | x == 'R' || x == 'L' = (max * 8) + getSeat (0, 7) (x : xs)
seatNr (min, _) ('F' : x : xs) | x == 'R' || x == 'L' = (min * 8) + getSeat (0, 7) (x : xs)
seatNr (min, max) ('B' : xs) = seatNr (min + chalf (max - min), max) xs
seatNr (min, max) ('F' : xs) = seatNr (min, max - chalf (max - min)) xs

findGap :: [Int] -> Int
findGap (x : y : xs) | y == x + 1 = findGap $ y : xs
findGap (x : _) = x + 1

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let passes = lines contents
  let seats = map (seatNr (0, 127)) passes

  -- print seats
  print $ foldl (\s n -> if n > s then n else s) 0 seats

  print $ findGap $ sort seats
