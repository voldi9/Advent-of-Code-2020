import Aoc

findMultiplier :: [Int] -> Int
findMultiplier i = f i (reverse i) where
  f (x:xs) (rx:rxs)
    | x + rx <  2020   = f   xs   (rx:rxs)
    | x + rx >  2020   = f (x:xs)   rxs
    | x + rx == 2020   = x * rx

main = do
  contents <- getContents
  putStrLn . show . findMultiplier . sort . map read . splitOnNewline $ contents
