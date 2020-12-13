import Data.List

splitOnNewline :: String -> [Int]
splitOnNewline s =
  map (read :: String -> Int) $
    filter (not . null) $
      foldr f [""] s where
        f c (x:xs)
          | c == '\n' = "":x:xs
          | otherwise = (c:x):xs

findMultiplier :: [Int] -> Int
findMultiplier i = findMultiplierHelper i (reverse i) where
  findMultiplierHelper (x:xs) (rx:rxs)
    | x + rx <  2020   = findMultiplierHelper   xs   (rx:rxs)
    | x + rx >  2020   = findMultiplierHelper (x:xs)   rxs
    | x + rx == 2020   = x * rx

result :: String -> Int
result l = findMultiplier $ sort $ splitOnNewline l

main = do
  contents <- getContents
  putStrLn $ show $ result contents
