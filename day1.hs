import Aoc

findMultiplierA :: [Int] -> Int
findMultiplierA i = f si (reverse si) where
  si = sort i
  f (x:xs) (rx:rxs)
    | x + rx <  2020   = f   xs   (rx:rxs)
    | x + rx >  2020   = f (x:xs)   rxs
    | x + rx == 2020   = x * rx

findMultiplierB :: [Int] -> Int
findMultiplierB (x:xs) =
  case f x xs of
    Nothing  -> findMultiplierB xs
    Just y   -> x * y
  where
    f c (x:xs)
      | 2020 - c - x `elem` xs = Just (x * (2020 - c - x))
      | otherwise              = f c xs
    f _ [] = Nothing

main = do
  contents <- getContents
  output findMultiplierA contents
  output findMultiplierB contents where
    output f = print . f . map read . splitOnNewline
