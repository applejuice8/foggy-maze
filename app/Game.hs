module Game (playGame) where

import System.IO
import Data.Char (toUpper)
import System.Console.ANSI (clearScreen)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import ScoreManager (Name, Score(..), writeScore)

-- Custom data types
type Maze = [[Tile]]
type Pos = (Int, Int)   -- (x, y)
type ColoredChar = String

data Tile = Wall | Exit | Player | Unknown | Empty
            deriving (Eq)

data Color = Green | Yellow | White | Gray | Reset

-- Data types conversion
charToTile :: Char -> Tile
charToTile c = case c of
    '#' -> Wall
    'E' -> Exit
    'P' -> Player
    '?' -> Unknown
    _   -> Empty

-- ANSI escape codes
colorCode :: Color -> String
colorCode c = case c of
    Green  -> "\ESC[92m"
    Yellow -> "\ESC[93m"
    White  -> "\ESC[37m"
    Gray   -> "\ESC[90m"
    _      -> "\ESC[0m"

tileToColoredChar :: Tile -> ColoredChar
tileToColoredChar t = case t of
        Wall    -> color White  "#"
        Exit    -> color Green  "E" 
        Player  -> color Yellow "P"
        Unknown -> color Gray   "?"
        _       -> " "
    where color c s = colorCode c ++ s ++ colorCode Reset

-- Maze template
initialMaze :: Maze
initialMaze =
    map (map charToTile)
        [ "#############"
        , "#P    #     #"
        , "### # ### # #"
        , "#   #     # #"
        , "# ### ##### #"
        , "#     #     #"
        , "# ### # ### #"
        , "# #   #   # #"
        , "# # ##### # #"
        , "#     #     E"
        , "#############"
        ]

-- Print maze
printMaze :: Maze -> Pos -> IO ()
printMaze maze playerPos = do
    putStrLn "Use WASD keys to find the exit"
    mapM_ (putStrLn . printRow playerPos) (zip [0..] maze)

-- Print a single row
printRow :: Pos -> (Int, [Tile]) -> ColoredChar
printRow (py, px) (y, row) =
        concat $ zipWith render [0..] row
    where
        render x tile =
            tileToColoredChar $
                if abs (y - py) <= 2 && abs (x - px) <= 2
                    then tile
                    else Unknown

-- Combine (row, col)
getPos :: Tile -> Maze -> Pos
getPos tile maze =
        (row, col)
    where
        row = getRowIndex tile maze 0
        col = getColIndex tile (maze !! row) 0

-- Find row in maze recursively
getRowIndex :: Tile -> Maze -> Int -> Int
getRowIndex _ [] _ = error "Tile not found in maze"
getRowIndex tile (row:rows) y
    | tile `elem` row = y
    | otherwise       = getRowIndex tile rows (y + 1)

-- Find col in a row recursively
getColIndex :: Tile -> [Tile] -> Int -> Int
getColIndex _ [] _ = error "Tile not found in row"
getColIndex tile (col:cols) x
    | col == tile = x
    | otherwise   = getColIndex tile cols (x + 1)

-- Handle movement key presses
handleMove :: Maze -> Char -> Maze
handleMove maze key =
    let oldPos = getPos Player maze
        newPos = tryNewPos key oldPos
    in if isValidPos maze newPos
        then movePlayer oldPos newPos maze
        else maze

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
        cols = length $ head maze

-- Move player from old to new position
movePlayer :: Pos -> Pos -> Maze -> Maze
movePlayer oldPos newPos maze =
    let mazeWithoutPlayer = replaceAt oldPos Empty maze
    in replaceAt newPos Player mazeWithoutPlayer

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

-- Check if a tile exists in maze
tileExists :: Maze -> Tile -> Bool
tileExists maze tile =
    any (elem tile) maze

-- Run when win
handleWin :: Name -> UTCTime -> IO ()
handleWin name startTime = do
    endTime <- getCurrentTime
    let time  = calcTimelapse startTime endTime
        file  = "app/scores.csv"
        score = Score name time

    putStrLn "You escaped!"
    putStrLn $ "Time taken: " ++ show time ++ " seconds"
    writeScore file score

-- Calculate timelapse
calcTimelapse :: UTCTime -> UTCTime -> Double
calcTimelapse start end =
    realToFrac $ diffUTCTime end start

-- Game loop
loop :: Maze -> Name -> UTCTime  -> IO ()
loop maze name startTime = do
    key <- getChar
    clearScreen
    let newMaze   = handleMove maze key
        playerPos = getPos Player newMaze
    printMaze newMaze playerPos

    if tileExists newMaze Exit
        then loop newMaze name startTime
        else handleWin name startTime

playGame :: Name -> IO ()
playGame name = do
    hSetBuffering stdin NoBuffering     -- Disable buffering (Key read immediately)
    hSetEcho stdin False    -- Don't print key entered

    clearScreen
    startTime <- getCurrentTime
    let playerPos = getPos Player initialMaze
    printMaze initialMaze playerPos

    loop initialMaze name startTime

    -- Restore terminal state
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
