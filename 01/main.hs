import System.IO
import Data.Char (ord)
import Data.Text (replace)
import Data.List (isPrefixOf, find)
import Data.Maybe (catMaybes)

numberChars = ['0'..'9']

isCharNumber :: Char -> Bool
isCharNumber ch = elem ch numberChars

extractNumbers :: String -> String
extractNumbers = filter isCharNumber

-- part 2 helpers
numberStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

numberStringToDigit :: String -> Char
numberStringToDigit str
  | str == "zero" = '0'
  | str == "one" = '1'
  | str == "two" = '2'
  | str == "three" = '3'
  | str == "four" = '4'
  | str == "five" = '5'
  | str == "six" = '6'
  | str == "seven" = '7'
  | str == "eight" = '8'
  | str == "nine" = '9'


matchingPrefix :: [String] -> String -> Maybe String
matchingPrefix words str = find (`isPrefixOf` str) words

-- func to traverse string until finding a token
findMatch :: String -> Maybe Char
findMatch [] = Nothing
findMatch [x] = if isCharNumber x then Just x else Nothing
findMatch list@(x:xs) = 
    case matchingWord of
      Just str -> Just (numberStringToDigit str)
      Nothing -> case findMatch [x] of
        Just y -> Just y
        Nothing -> findMatch xs
  where
    matchingWord = matchingPrefix numberStrings list

sumMaybeTuples :: (Maybe Char, Maybe Char) -> Maybe Int
sumMaybeTuples (Just ch1, Just ch2) = Just (read ([ch1] ++ [ch2]))
sumMaybeTuples _ = Nothing

-- From root of repository:
-- runhaskell 01/main.hs
main = do
  handle <- openFile "01/input.txt" ReadMode
  contents <- hGetContents handle

  let lists = lines contents

  -- part 1
  let numlines = map extractNumbers lists
  let digitsToSum = map (\line -> read([head line] ++ [last line])) numlines
  let total = sum digitsToSum
  print "Part 1"
  print total

  -- part 2
  let numPairs = map (\line -> (findMatch line, findMatch (reverse line))) lists
  -- print numLinesPart2
  let maybeSums = map sumMaybeTuples numPairs
  print maybeSums
  let sums = catMaybes maybeSums
  print sums
  let totalPart2 = sum sums
  print "Part 2"
  print totalPart2

  hClose handle