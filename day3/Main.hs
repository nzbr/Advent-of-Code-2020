module Main where

-- http://stackoverflow.com/questions/4978578/ddg#4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

endless :: [a] -> [a]
endless list = list ++ endless list

slide :: (Int, Int) -> Int -> [[Bool]] -> [Bool]
slide _ _ [] = []
slide (r, d) i (x : xs) = x !! i : slide (r, d) (i + r) (drop (d - 1) xs)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines = wordsWhen (== '\n') contents
  let treemap = map (endless . map (== '#')) lines
  let result = foldl (\i b -> if b then i + 1 else i) 0 $ slide (3, 1) 0 treemap
  print result
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let results = map (\slope -> foldl (\i b -> if b then i + 1 else i) 0 $ slide slope 0 treemap) slopes
  print results
  print $ product results
