{-# LANGUAGE LambdaCase #-}     -- For lambda case

module Convert where

import Text.Read (readMaybe)
import Types

-- Game.hs
charToTile :: Char -> Tile
charToTile = \case
    '#' -> Wall
    'E' -> Exit
    'P' -> Player
    '?' -> Unknown
    _   -> Empty

charToDirection :: Char -> Maybe Direction
charToDirection = \case
    'W' -> Just UpDir
    'A' -> Just LeftDir
    'S' -> Just DownDir
    'D' -> Just RightDir
    _   -> Nothing

diffToSize :: Difficulty -> Int
diffToSize = \case
    Easy   -> 4     -- 9x9 tiles
    Medium -> 3     -- 7x7 tiles
    Hard   -> 2     -- 5x5 tiles
    Insane -> 1     -- 3x3 tiles

colorCode :: Color -> String
colorCode = \case
        Green  -> ansi "92"
        Yellow -> ansi "93"
        White  -> ansi "37"
        Gray   -> ansi "90"
        _      -> ansi "0"
    where
        ansi code = "\ESC[" ++ code ++ "m"

tileToColoredChar :: Tile -> ColoredChar
tileToColoredChar = \case
        Wall    -> color White  "#"
        Exit    -> color Green  "E" 
        Player  -> color Yellow "P"
        Unknown -> color Gray   "?"
        _       -> " "
    where
        color c s = colorCode c ++ s ++ colorCode Reset

-- ScoreManager.hs
scoreToRow :: Score -> Row
scoreToRow (Score name diff time) =
    [name, show diff, show time]

rowToScore :: Row -> Maybe Score
rowToScore [name, diffStr, timeStr] =
    readMaybe diffStr >>= \diff ->
    readMaybe timeStr >>= \time ->
        return $ Score name diff time
rowToScore _ = Nothing
