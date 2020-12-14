module Main where

valid :: [Int] -> Int -> Bool
valid pre i = i `elem` [a + b | a <- pre, b <- pre]

mapValid :: [Int] -> [Int] -> [(Int, Bool)]
mapValid _ [] = []
mapValid pre (i : nxt) = (i, valid pre i) : mapValid (i : init pre) nxt

contiguousSets :: [Int] -> [[Int]]
contiguousSets [] = []
contiguousSets (x : xs) = continue [x] xs ++ contiguousSets xs
  where
    continue :: [Int] -> [Int] -> [[Int]]
    continue s [] = [reverse s]
    continue s (x : xs) = reverse s : continue (x : s) xs

main :: IO ()
main = do
  text <- readFile "input.txt"
  let nums = map (\x -> read x :: Int) $ lines text
  let prelen = 25

  -- Part 1 --
  let invalidnr = fst $ head $ filter ((==) False . snd) $ mapValid (reverse $ take prelen nums) (drop prelen nums)
  print invalidnr

  -- Part 2 --
  let candidates = [x | x <- contiguousSets nums, length x >= 2]
  let weakness = head $ filter ((==) invalidnr . sum) candidates
  print weakness
  print $ minimum weakness + maximum weakness
