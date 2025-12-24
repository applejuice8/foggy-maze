{-# LANGUAGE LambdaCase #-}     -- For lambda case

module ScoreManager (writeScore, readScores, topNScores) where

import System.Directory (doesFileExist)
import Text.CSV (parseCSVFromFile, printCSV)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Types (Score(..), Row)
import Convert (scoreToRow, rowToScore)

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
topNScores :: Ord a => Int -> [a] -> [a]
topNScores n = take n . sort
