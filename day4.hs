import Aoc

data Field = F String String deriving Show

required = ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

isValid :: Field -> Bool
isValid (F "ecl" s) = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValid (F "hgt" s) = matches "^1(([5-8][0-9])|(9[0-3]))cm$" s || matches "^((59)|(6[0-9])|(7[0-6]))in$" s
isValid (F "pid" s) = matches "^[0-9]{9}$" s
isValid (F "hcl" s) = matches "^#[0-9a-f]{6}$" s
isValid (F "byr" s) = between 1920 2002 s
isValid (F "iyr" s) = between 2010 2020 s
isValid (F "eyr" s) = between 2020 2030 s
isValid _           = False

matches r v = matchRegex (mkRegex r) v /= Nothing
between s e v = p /= -1 && p >= s && p <= e where p = fromMaybe (-1) (readMaybe v :: Maybe Int)

parseRegexps :: String -> [Field]
parseRegexps s = map (f s . rgx) required where
  rgx s = "(" ++ s ++ ":[^ \n]+)"
  f s r = case take 2 . splitOn ":" . head . fromMaybe [""] $ (matchRegex . mkRegex) r s of
    [x]      -> F x x
    (f:v:xs) -> F f v

main = do
  contents <- getContents
  let rows = splitOn "\n\n" contents
  print . count True $ map f rows
  print . count True $ map (and . map isValid . parseRegexps) rows
  where
    f l = all (`isInfixOf` l) required
    xd row = print "a"
