module Game (Name, playGame) where

import System.IO
import Data.Char (toUpper)
import System.Console.ANSI (clearScreen)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import ScoreManager (Score(..), writeScore)

-- Custom data types
type Name = String
type Maze = [[Tile]]
type Pos = (Int, Int)   -- (x, y)

data Tile
    = Wall
    | Empty
    | Exit
    | Player
    deriving (Eq)

-- Data types conversion
charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile ' ' = Empty
charToTile 'E' = Exit
charToTile '&' = Player
charToTile _   = Empty

tileToChar :: Tile -> Char
tileToChar Wall   = '#'
tileToChar Empty  = ' '
tileToChar Exit   = 'E'
tileToChar Player = '&'

-- Sample Maze
initialMaze :: Maze
initialMaze =
    map (map charToTile)
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
printMaze =
    mapM_ (putStrLn . map tileToChar)

-- Find col in a row recursively
getColIndex :: Tile -> [Tile] -> Int -> Int
getColIndex _ [] _ = error "Tile not found in row"
getColIndex tile (col:cols) x
    | col == tile = x
    | otherwise = getColIndex tile cols (x + 1)

-- Find row in maze recursively
getRowIndex :: Tile -> Maze -> Int -> Int
getRowIndex _ [] _ = error "Tile not found in maze"
getRowIndex tile (row:rows) y
    | tile `elem` row = y
    | otherwise = getRowIndex tile rows (y + 1)

-- Combine (row, col)
getPos :: Tile -> Maze -> Pos
getPos tile maze =
        (row, col)
    where
        row = getRowIndex tile maze 0
        col = getColIndex tile (maze !! row) 0

-- Top, Left, Bottom, Right
tryNewPos :: Char -> Pos -> Pos
tryNewPos key (y, x) = 
    case toUpper key of
        'W' -> (y - 1, x)
        'A' -> (y, x - 1)
        'S' -> (y + 1, x)
        'D' -> (y, x + 1)
        _   -> (y, x)

-- Check if new position is a wall / exceeded
isValidPos :: Maze -> Pos -> Bool
isValidPos maze (y, x) =
        y >= 0 && y < rows &&
        x >= 0 && x < cols &&
        maze !! y !! x /= Wall
    where
        rows = length maze
        cols = length (head maze)

-- Check if a tile exists in maze
tileExists :: Maze -> Tile -> Bool
tileExists maze tile =
    any (elem tile) maze

-- Replace a tile in maze
replaceAt :: Pos -> Tile -> Maze -> Maze
replaceAt (y, x) newTile maze =
    case splitAt y maze of
        (above, row:below) ->
            above ++ replaceTile x newTile row : below
        _ -> maze   -- y out of bounds

-- Replace a tile in a row
replaceTile :: Int -> Tile -> [Tile] -> [Tile]
replaceTile x newTile row =
    case splitAt x row of
        (before, _:after) ->
            before ++ newTile : after
        _ -> row

-- Move player from old to new position
movePlayer :: Pos -> Pos -> Maze -> Maze
movePlayer oldPos newPos maze =
    let mazeWithoutPlayer = replaceAt oldPos Empty maze
    in replaceAt newPos Player mazeWithoutPlayer

-- Handle movement key presses
handleMove :: Maze -> Char -> Maze
handleMove maze key =
    let oldPos = getPos Player maze
        newPos = tryNewPos key oldPos
    in if isValidPos maze newPos
        then movePlayer oldPos newPos maze
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

    if tileExists newMaze Exit
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
