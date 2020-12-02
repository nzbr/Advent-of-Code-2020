module Main where

import Prelude

-- a --
pairs :: [Integer] -> [(Integer, Integer)]
pairs list = [(x, y) | x <- list, y <- list]

getResultA :: [(Integer, Integer)] -> Integer
getResultA [] = -1
getResultA ((x, y) : _) | x + y == 2020 = x * y
getResultA (_ : xs) = getResultA xs

-- b --
triples :: [Integer] -> [(Integer, Integer, Integer)]
triples list = [(x, y, z) | x <- list, y <- list, z <- list]

getResultB :: [(Integer, Integer, Integer)] -> Integer
getResultB [] = -1
getResultB ((x, y, z) : _) | x + y + z == 2020 = x * y * z
getResultB (_ : xs) = getResultB xs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ints = map (\it -> read it :: Integer) $ words contents

  -- a --
  print $ getResultA $ pairs ints

  -- b --
  print $ getResultB $ triples ints
