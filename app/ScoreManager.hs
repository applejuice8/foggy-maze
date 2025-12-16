module ScoreManager
    ( Score(..)
    , writeScore
    , topScores
    , readScores
    ) where

import System.Directory (doesFileExist)
import Text.CSV (parseCSVFromFile, printCSV)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

type Name = String
type Seconds = Double

data Score = Score
    { playerName :: Name
    , timeTaken  :: Seconds
    } deriving (Show, Eq, Ord)

scoreToRecord :: Score -> [String]
scoreToRecord (Score name time) =
    [name, show time]

recordToScore :: [String] -> Maybe Score
recordToScore [name, timeStr] = do
    secs <- readMaybe timeStr
    Just (Score name secs)
recordToScore _ = Nothing

-- Read csv file if exist
readCSV :: FilePath -> IO [[String]]
readCSV file = do
    exists <- doesFileExist file
    if not exists
        then return []
        else do
            result <- parseCSVFromFile file
            case result of
                Right rows -> return rows
                Left _     -> return []

-- Check if a row is empty
isEmptyRow :: [String] -> Bool
isEmptyRow [] = True
isEmptyRow fields = all null fields

-- Filter empty rows
cleanRows :: [[String]] -> [[String]]
cleanRows = filter (not . isEmptyRow)

-- Write scores to csv file
writeScore :: FilePath -> Score -> IO ()
writeScore file score = do
    rows <- readCSV file
    let validRows = cleanRows rows
        newRows = validRows ++ [scoreToRecord score]
    writeFile file (printCSV newRows)

-- Read scores from csv file
readScores :: FilePath -> IO [Score]
readScores file = do
    rows <- readCSV file
    return (mapMaybe recordToScore rows)

-- Take top n scores
topScores :: Int -> [Score] -> [Score]
topScores n =
    take n . sortOn timeTaken

-- main :: IO ()
-- main = do
--     let file = "app/scores.csv"

--     writeScore file (Score "Colin" 12.38)
--     writeScore file (Score "Alice" 9.12)

--     scores <- readScores file
--     mapM_ print (topScores 5 scores)
