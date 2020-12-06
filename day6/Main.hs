module Main where

import Data.List

splitOnEmptyLine :: String -> [String]
splitOnEmptyLine [] = []
splitOnEmptyLine [x] = [[x]]
splitOnEmptyLine ('\n' : '\n' : xs) = [] : splitOnEmptyLine xs
splitOnEmptyLine (x : y : xs) = (x : head (splitOnEmptyLine (y : xs))) : tail (splitOnEmptyLine (y : xs))

uniq :: String -> String
uniq [] = ""
uniq [x] = [x]
uniq (x : y : xs) | x == y = uniq (x : xs)
uniq (x : y : xs) = x : uniq (y : xs)

inAll :: String -> [String] -> String
inAll s [] = s
inAll s (x : xs) = filter (\c -> c `elem` inAll s xs) x

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ sum $ map (length . uniq . sort . concat . lines) $ splitOnEmptyLine contents
  print $ sum $ map (length . inAll "abcdefghijklmnopqrstuvwxyz" . lines) $ splitOnEmptyLine contents
