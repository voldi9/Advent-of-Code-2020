import Aoc

data PWCheck = P Int Int Char String

parseLine :: String -> PWCheck
parseLine s = P l1 l2 (head lt) pw where
  [l1, l2] = map read . splitOn '-' $ ln
  [ln, lt, pw] = splitOn ' ' s


isValidA :: String -> Bool
isValidA s = f . parseLine $ s where
  f (P mn mx c pw) = not $ mn > countItem c pw || mx < countItem c pw

isValidB :: String -> Bool
isValidB s = f . parseLine $ s where
  f (P l1 l2 c pw) = xor (elem l1 pos) (elem l2 pos) where
    pos = [x + 1 | x <- [0..length pw - 1], pw !! x == c]

main = do
  contents <- getContents
  output isValidA contents
  output isValidB contents where
    output f = print . countItem True . map f . splitOnNewline
