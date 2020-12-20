import Aoc

result :: [String] -> (Int, Int) -> Int
result ls (r, d) = (count True) $ map f [(r*i, d*i) | i <- [0..(length ls - 1) `div` d]] where
  f (x, y) = let row = ls !! y
             in row !! (x `mod` length row) == '#'

main = do
  contents <- getContents
  let resRows = result (splitOnNewline contents)
  print $ resRows (3,1)
  print $ foldl (*) 1 $ map resRows [(1,1), (3,1), (5,1), (7,1), (1,2)]
