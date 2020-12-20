import Aoc

data PWCheck = P Int Int Char String

parseLine :: String -> PWCheck
parseLine s = P l1 l2 (head lt) pw where
  [l1, l2] = map read . splitOn '-' $ ln
  [ln, lt, pw] = splitOn ' ' s

isValidA :: String -> Bool
isValidA s = f . parseLine $ s where
  f (P mn mx c pw) = let occ = count c pw
                     in not $ mn > occ || mx < occ

isValidB :: String -> Bool
isValidB s = f . parseLine $ s where
  f (P l1 l2 c pw) = xor (pw !! (l1 - 1) == c) (pw !! (l2 - 1) == c)

main = do
  contents <- getContents
  output isValidA contents
  output isValidB contents where
    output f = print . count True . map f . splitOnNewline
