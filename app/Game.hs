module Game (Name, playGame) where

import System.IO
import Data.Char (toUpper)
import System.Console.ANSI (clearScreen)
import Data.Time.Clock
import ScoreManager (Score(..), writeScore)

-- Custom data types
type Name = String
type Maze = [String]
type Pos = (Int, Int)   -- (x, y)

-- Sample Maze
initialMaze :: Maze
initialMaze =
    [ "#########"
    , "#       E"
    , "# ### # #"
    , "# #   # #"
    , "# # ### #"
    , "#&      #"
    , "#########"
    ]

-- putStrLn for each list item
printMaze :: Maze -> IO ()
printMaze = mapM_ putStrLn

-- Find col in a row recursively
getColIndex :: Char -> String -> Int -> Int
getColIndex _ [] _ = error "Symbol not found in row"
getColIndex symbol (col:cols) x
    | col == symbol = x
    | otherwise = getColIndex symbol cols (x + 1)

-- Find row in maze recursively
getRowIndex :: Char -> Maze -> Int -> Int
getRowIndex _ [] _ = error "Symbol not found in maze"
getRowIndex symbol (row:rows) y
    | symbol `elem` row = y
    | otherwise = getRowIndex symbol rows (y + 1)

-- Combine (row, col)
getPos :: Char -> Maze -> Pos
getPos symbol maze =
        (row, col)
    where
        row = getRowIndex symbol maze 0
        col = getColIndex symbol (maze !! row) 0

-- Top, Left, Bottom, Right
tryNewPos :: Char -> Pos -> Pos
tryNewPos key (y, x) = 
    case toUpper key of
        'W' -> (y - 1, x)
        'A' -> (y, x - 1)
        'S' -> (y + 1, x)
        'D' -> (y, x + 1)
        _ -> (y, x)

-- Check if new position is a wall / exceeded
isValidPos :: Maze -> Pos -> Bool
isValidPos maze (y, x) =
        y >= 0 && y < rows &&
        x >= 0 && x < cols &&
        maze !! y !! x /= '#'
    where
        rows = length maze
        cols = length (head maze)

symbolExists :: Maze -> Char -> Bool
symbolExists maze symbol = any (elem symbol) maze

-- Replace a char in maze
replaceAt :: Pos -> Char -> Maze -> Maze
replaceAt (y, x) newChar maze =
    case splitAt y maze of
        (above, row:below) ->
            above ++ replaceChar x newChar row : below
        _ ->
            maze    -- y out of bounds

-- Replace a char in a row
replaceChar :: Int -> Char -> String -> String
replaceChar x newChar row =
    case splitAt x row of
        (before, _:after) ->
            before ++ newChar : after
        _ ->
            row

-- Move player from old to new position
movePlayer :: Pos -> Pos -> Maze -> Char -> Maze
movePlayer oldPos newPos maze symbol =
    let mazeWithoutPlayer = replaceAt oldPos ' ' maze
    in replaceAt newPos symbol mazeWithoutPlayer

-- Handle movement key presses
handleMove :: Maze -> Char -> Maze
handleMove maze key =
    let oldPos = getPos '&' maze
        newPos = tryNewPos key oldPos
    in if isValidPos maze newPos
        then movePlayer oldPos newPos maze '&'
        else maze

-- Calculate timelapse
calcTimelapse :: UTCTime -> UTCTime -> Double
calcTimelapse start end =
    realToFrac (diffUTCTime end start)

-- Run when win
handleWin :: Name -> UTCTime -> IO ()
handleWin name startTime = do
    endTime <- getCurrentTime
    let time = calcTimelapse startTime endTime
        file = "app/scores.csv"
        score = Score name time

    putStrLn "You escaped!"
    putStrLn ("Time taken: " ++ show time ++ " seconds")
    writeScore file score

-- Game loop
loop :: Maze -> Name -> UTCTime  -> IO ()
loop maze name startTime = do
    key <- getChar
    let newMaze = handleMove maze key

    clearScreen
    printMaze newMaze

    if symbolExists newMaze 'E'
        then loop newMaze name startTime
        else handleWin name startTime

playGame :: Name -> IO ()
playGame name = do
    hSetBuffering stdin NoBuffering     -- Disable buffering (Key read immediately)
    hSetEcho stdin False    -- Don't print key entered

    startTime <- getCurrentTime
    printMaze initialMaze
    loop initialMaze name startTime

    -- Restore terminal state
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
