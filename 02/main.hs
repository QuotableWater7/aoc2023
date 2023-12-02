import System.IO
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (isSuffixOf, find)

data Turn = Turn { getRed :: Int, getGreen :: Int, getBlue :: Int } deriving(Show)

data Game = Game { getIndex :: Int, getTurns :: [Turn] } deriving(Show)

parseGame :: String -> Game
parseGame str = Game { getIndex = index, getTurns = turns }
  where
    (gameDef, rest) = splitOnGameIndexDefinition str
    index = extractNumberFromStart (drop 5 gameDef)
    turnStrings = splitOn "; " rest
    turns = map parseTurn turnStrings

parseTurn :: String -> Turn
parseTurn string = Turn { getRed = red, getGreen = green, getBlue = blue }
  where
    colorStrings = splitOn ", " string
    redString = find (\s -> "red" `isSuffixOf` s) colorStrings
    red = case redString of
      Just str -> extractNumberFromStart str
      Nothing -> 0
    greenString = find (\s -> "green" `isSuffixOf` s) colorStrings
    green = case greenString of
      Just str -> extractNumberFromStart str
      Nothing -> 0
    blueString = find (\s -> "blue" `isSuffixOf` s) colorStrings
    blue = case blueString of
      Just str -> extractNumberFromStart str
      Nothing -> 0

splitOnGameIndexDefinition :: String -> (String, String)
splitOnGameIndexDefinition str =
    let (before, after) = break (== ':') str
    in if null after
       then (before, "")
       else (before, drop 2 after) -- drop 2 to remove the ": " from the start of 'after'

extractNumberFromStart :: String -> Int
extractNumberFromStart str = read numberStr
  where 
    numberStr = takeWhile (isDigit) $ str

isValidTurn :: (Int, Int, Int) -> Turn -> Bool
isValidTurn (red, green, blue) turn = isRedValid && isGreenValid && isBlueValid
  where
    isRedValid = red >= getRed turn
    isBlueValid = blue >= getBlue turn
    isGreenValid = green >= getGreen turn
    
isValidGame :: (Int, Int, Int) -> Game -> Bool
isValidGame validityTuple game = all (isValidTurn validityTuple) (getTurns game)

sumValidGameIndices :: (Int, Int, Int) -> [Game] -> Int
sumValidGameIndices validityTuple games = sum indices
  where
    validGames = filter (isValidGame validityTuple) games
    indices = map getIndex validGames

getPowerOfCubes :: Game -> Int
getPowerOfCubes game = maxRed * maxGreen * maxBlue
  where
    turns = getTurns game
    maxRed = maximum (map getRed turns)
    maxGreen = maximum (map getGreen turns)
    maxBlue = maximum (map getBlue turns)
    
main = do
  handle <- openFile "02/input.txt" ReadMode
  contents <- hGetContents handle

  -- part 1
  let rawGames = lines contents
  let games = map parseGame rawGames
  print "Part 1"
  print $ sumValidGameIndices (12, 13, 14) games

  -- part 2
  print "Part 2"
  print $ sum (map getPowerOfCubes games)

  hClose handle