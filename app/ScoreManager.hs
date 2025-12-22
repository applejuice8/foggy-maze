{-# LANGUAGE LambdaCase #-}     -- For lambda case

module ScoreManager
    ( Name, Difficulty(..), Score(..),
    writeScore, readScores, topScores
    ) where

import System.Directory (doesFileExist)
import Text.CSV (parseCSVFromFile, printCSV)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- Custom data types
type Name    = String
type Seconds = Double
type Row     = [String]

data Difficulty = Easy | Medium | Hard | Insane
                deriving (Show, Read, Eq)

-- Reverse default rankings
instance Ord Difficulty where
    compare a b = compare (rank a) (rank b)
        where
            rank :: Difficulty -> Int
            rank Easy   = 4
            rank Medium = 3
            rank Hard   = 2
            rank Insane = 1

data Score = Score
    { playerName :: Name
    , difficulty :: Difficulty
    , timeTaken  :: Seconds
    } deriving Show

instance Eq Score where
    a == b =
        timeTaken a  == timeTaken b &&
        difficulty a == difficulty b

-- Sort by timeTaken, then by difficulty
instance Ord Score where
    compare a b =
        compare (timeTaken a) (timeTaken b) <>
        compare (difficulty a) (difficulty b)

-- Data types conversion
scoreToRow :: Score -> Row
scoreToRow (Score name diff time) =
    [name, show diff, show time]

rowToScore :: Row -> Maybe Score
rowToScore [name, diffStr, timeStr] =
    readMaybe diffStr >>= \diff ->
    readMaybe timeStr >>= \time ->
        return $ Score name diff time
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

-- Filter empty rows
cleanRows :: [Row] -> [Row]
cleanRows = filter $ not . isEmptyRow

-- Check if a row is empty
isEmptyRow :: Row -> Bool
isEmptyRow []     = True
isEmptyRow fields = all null fields

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
topScores n = take n . sort
