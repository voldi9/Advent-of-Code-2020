import Aoc
import Debug.Trace

findMultiplier :: [Int] -> Int
findMultiplier (x:xs) =
  case f x xs of
    Nothing  -> findMultiplier xs
    Just y   -> x * y
  where
    f c (x:xs)
      | 2020 - c - x `elem` xs = Just (x * (2020 - c - x))
      | otherwise              = f c xs
    f _ [] = Nothing

main = do
  contents <- getContents
  putStrLn . show . findMultiplier . map read . splitOnNewline $ contents
