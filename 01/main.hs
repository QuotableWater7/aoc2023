import System.IO
import Data.Char (ord)
import Data.Text (replace)

ord0 = ord '0'
ord9 = ord '9'

isCharNumber :: Char -> Bool
isCharNumber x = ordX >= ord0 && ordX <= ord9
  where
    ordX = ord x

extractNumbers :: String -> String
extractNumbers = filter isCharNumber

-- part 2 helpers

-- func to traverse string until finding a token

-- From root of repository:
-- runhaskell 01/main.hs
main = do
  handle <- openFile "01/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let numlines = map extractNumbers (lines contents)
  let digitsToSum = map (\line -> read([head line] ++ [last line])) numlines
  let total = sum digitsToSum
  print "Part 1"
  print total

  -- part 2

  hClose handle