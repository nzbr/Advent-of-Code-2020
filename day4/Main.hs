module Main where

import Data.Bifunctor
import Data.List (isSuffixOf, sortOn)
import Text.Regex.TDFA ((=~))

splitOnEmptyLine :: String -> [String]
splitOnEmptyLine [] = []
splitOnEmptyLine [x] = [[x]]
splitOnEmptyLine ('\n' : '\n' : xs) = [] : splitOnEmptyLine xs
splitOnEmptyLine (x : y : xs) = (x : head (splitOnEmptyLine (y : xs))) : tail (splitOnEmptyLine (y : xs))

key :: String -> String
key [] = []
key (':' : _) = []
key (x : xs) = (:) x $ key xs

keyvalue :: String -> (String, String)
keyvalue [] = ([], [])
keyvalue (':' : xs) = ([], xs)
keyvalue (x : xs) = first (x :) (keyvalue xs)

validA :: [String] -> Bool
validA keys = "byr" `elem` keys && "iyr" `elem` keys && "eyr" `elem` keys && "hgt" `elem` keys && "hcl" `elem` keys && "ecl" `elem` keys && "pid" `elem` keys

validB :: [(String, String)] -> Bool
validB input = all matchFn input && validA (map fst input)
  where
    int x = if x =~ "[0-9]+" then (read x :: Int) else -1
    matchFn :: (String, String) -> Bool
    matchFn (k, x) = case k of
      "byr" -> int x >= 1920 && int x <= 2002
      "iyr" -> int x >= 2010 && int x <= 2020
      "eyr" -> int x >= 2020 && int x <= 2030
      "hgt" | "cm" `isSuffixOf` x -> int (init $ init x) >= 150 && int (init $ init x) <= 193
      "hgt" | "in" `isSuffixOf` x -> int (init $ init x) >= 59 && int (init $ init x) <= 76
      "hgt" | otherwise -> False
      "hcl" -> x =~ "^#[0-9a-f]{6}$" :: Bool
      "ecl" -> x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      "pid" -> x =~ "^[0-9]{9}$" :: Bool
      "cid" -> True

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x : xs) = do
  print x
  printAll xs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ count $ map (validA . map key . words) $ splitOnEmptyLine contents
  -- DEBUG: printAll $ map (sortOn fst . filter ((\b -> b /= "cid" && b /= "hgt") . fst)) $ filter validB $ map (map keyvalue . words) $ splitOnEmptyLine contents
  print $ count $ map (validB . map keyvalue . words) $ splitOnEmptyLine contents
  where
    count :: [Bool] -> Int
    count = foldl (\i b -> if b then i + 1 else i) 0
