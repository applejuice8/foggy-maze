{-# LANGUAGE LambdaCase #-}     -- For lambda case

module ScoreManager
    ( Name, Score(..), writeScore     -- Game.hs
    , readScores, topScores     -- Main.hs
    ) where

import System.Directory (doesFileExist)
import Text.CSV (parseCSVFromFile, printCSV)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- Custom data types
type Name    = String
type Seconds = Double
type Row     = [String]

data Score = Score
    { playerName :: Name
    , timeTaken  :: Seconds
    } deriving (Show, Eq, Ord)

-- Data types conversion
scoreToRow :: Score -> Row
scoreToRow (Score name time) =
    [name, show time]

rowToScore :: Row -> Maybe Score
rowToScore [name, timeStr] =
    readMaybe timeStr >>= \secs ->
    Just (Score name secs)
rowToScore _ = Nothing

-- Read csv file if exist
readCSV :: FilePath -> IO [Row]
readCSV file =
    doesFileExist file >>= \exists ->
        if not exists
            then return []
            else
                parseCSVFromFile file >>= \case
                    Right rows -> return rows
                    Left _     -> return []

-- Check if a row is empty
isEmptyRow :: Row -> Bool
isEmptyRow []     = True
isEmptyRow fields = all null fields

-- Filter empty rows
cleanRows :: [Row] -> [Row]
cleanRows = filter $ not . isEmptyRow

-- Write scores to csv file
writeScore :: FilePath -> Score -> IO ()
writeScore file score =
    readCSV file >>= \rows ->
        let validRows = cleanRows rows
            newRows   = validRows ++ [scoreToRow score]
        in writeFile file $ printCSV newRows

-- Read scores from csv file
readScores :: FilePath -> IO [Score]
readScores file =
    readCSV file >>= \rows ->
        return $ mapMaybe rowToScore rows

-- Take top n scores
topScores :: Int -> [Score] -> [Score]
topScores n =
    take n . sortOn timeTaken
