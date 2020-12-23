import Aoc

main = do
  contents <- getContents
  let sortedIds = sort $ map (foldl (\x y -> 2 * x + matchDir y) 0) (splitOnNewline contents)
  print . last $ sortedIds
  print . findSeat $ sortedIds
  where
    findSeat (l:r:xs)
      | l + 1 < r = l + 1
      | otherwise = findSeat (r:xs)
    matchDir c
      | c == 'B' || c == 'R' = 1
      | c == 'F' || c == 'L' = 0
