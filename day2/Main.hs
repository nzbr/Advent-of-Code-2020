module Main where

import Text.Regex.TDFA ((=~))
import Prelude

-- http://stackoverflow.com/questions/4978578/ddg#4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

parse :: String -> (Integer, Integer, Char, String)
parse s = cast matches
  where
    cast [min, max, chr, pass] = (read min :: Integer, read max :: Integer, head chr, pass)
    matches = submatches (s =~ "(.+)-(.+) (.): (.*)" :: (String, String, String, [String]))
      where
        submatches (_, _, _, x) = x

valid :: (Integer, Integer, Char, String) -> Bool
valid (min, max, chr, pass) = chrcount >= min && chrcount <= max
  where
    chrcount = foldl (\i c -> if c == chr then i + 1 else i) 0 pass

validB :: (Integer, Integer, Char, String) -> Bool
validB (first, second, chr, pass) = validBInt (fromIntegral first, fromIntegral second, chr, pass)
  where
    validBInt (first, second, chr, pass) = ((pass !! (-) first 1) == chr) /= ((pass !! (-) second 1) == chr)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines = wordsWhen (== '\n') contents
  let parsed = map parse lines
  print $ foldl (\i line -> if valid line then i + 1 else i) 0 parsed
  print $ foldl (\i line -> if validB line then i + 1 else i) 0 parsed
