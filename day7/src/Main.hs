module Main where

import Data.HashMap (Map, empty, insert, (!))
import Text.Regex.TDFA ((=~))

splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma [x] = [[x]]
splitOnComma (',' : ' ' : xs) = [] : splitOnComma xs
splitOnComma (x : y : xs) = (x : head (splitOnComma (y : xs))) : tail (splitOnComma (y : xs))

parse :: String -> (String, [(Int, String)])
parse s = container $ (s =~ "(.*) bags contain (.*)\\." :: (String, String, String, [String]))
  where
    container (_, _, _, [c, x]) = (c, contained x)
    contained "no other bags" = []
    contained s = map (extractInt . (\s -> s =~ "([0-9]*) (.*) bags?" :: (String, String, String, [String]))) $ splitOnComma s
      where
        extractInt (_, _, _, [i, s]) = (read i :: Int, s)

mkmap :: [(String, [(Int, String)])] -> Map String [(Int, String)]
mkmap [] = empty
mkmap ((k, v) : xs) = insert k v (mkmap xs)

canContain :: String -> Map String [(Int, String)] -> [(Int, String)] -> Bool
canContain _ _ [] = False
canContain target m ((_, bag) : xs) = bag == target || canContain target m xs || canContain target m (m ! bag)

countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue (True : xs) = 1 + countTrue xs
countTrue (False : xs) = countTrue xs

countContent :: Map String [(Int, String)] -> [(Int, String)] -> Int
countContent _ [] = 0
countContent m ((i, bag) : xs) = i + i * (countContent m (m ! bag)) + countContent m xs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let parsed = map parse $ lines contents
  let bagmap = mkmap parsed
  print $ countTrue $ map (canContain "shiny gold" bagmap . snd) parsed
  print $ countContent bagmap (bagmap ! "shiny gold")
