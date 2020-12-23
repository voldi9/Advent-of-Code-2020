import Aoc
import qualified Data.Set as Set

main = do
  contents <- getContents
  print . sum $ map (length . Set.fromList . filter isLower) (splitOnBlankLine contents)
  print . sum $ map (length . hasLetters) (splitOnBlankLine contents) where
    hasLetters s = filter (allRows . splitOnNewline $ s) ['a'..'z']
    allRows r c = and $ map (elem c) r
