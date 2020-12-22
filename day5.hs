import Aoc

main = do
  contents <- getContents
  print . maximum $ map (foldl (\x y -> 2 * x + mp y) 0) (splitOnNewline contents) where
    mp c
      | c == 'B' || c == 'R' = 1
      | c == 'F' || c == 'L' = 0
