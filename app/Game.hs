{-# LANGUAGE LambdaCase #-}     -- For lambda case

module Game (playGame) where

import System.IO
import Data.Char (toUpper)
import System.Console.ANSI (clearScreen)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Config (scoresFile)
import ScoreManager (Name, Difficulty(..), Score(..), writeScore)

-- Custom data types
type Maze = [[Tile]]
type Pos = (Int, Int)   -- (x, y)
type ColoredChar = String

data Tile = Wall | Exit | Player | Unknown | Empty
            deriving (Eq)

data Color = Green | Yellow | White | Gray | Reset

data GameState = GameState
    { gsMaze :: Maze
    , gsName :: Name
    , gsDiff :: Difficulty
    , gsStartTime :: UTCTime
    , gsPos :: Pos
    }

-- Data types conversion
charToTile :: Char -> Tile
charToTile = \case
    '#' -> Wall
    'E' -> Exit
    'P' -> Player
    '?' -> Unknown
    _   -> Empty

diffToSize :: Difficulty -> Int
diffToSize = \case
    Easy   -> 4     -- 9x9 tiles
    Medium -> 3     -- 7x7 tiles
    Hard   -> 2     -- 5x5 tiles
    Insane -> 1     -- 3x3 tiles

-- ANSI escape codes
colorCode :: Color -> String
colorCode = \case
    Green  -> "\ESC[92m"
    Yellow -> "\ESC[93m"
    White  -> "\ESC[37m"
    Gray   -> "\ESC[90m"
    _      -> "\ESC[0m"

tileToColoredChar :: Tile -> ColoredChar
tileToColoredChar = \case
        Wall    -> color White  "#"
        Exit    -> color Green  "E" 
        Player  -> color Yellow "P"
        Unknown -> color Gray   "?"
        _       -> " "
    where
        color c s = colorCode c ++ s ++ colorCode Reset

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
printMaze :: GameState -> IO ()
printMaze gs =
    putStrLn "Use WASD keys to find the exit" >>

    let maze = gsMaze gs
        (py, px) = gsPos gs
        size = diffToSize $ gsDiff gs

    in mapM_ putStrLn
        [ concat
            [ tileToColoredChar $
                if abs (y - py) <= size && abs (x - px) <= size
                    then tile
                    else Unknown
            | (x, tile) <- zip [0..] row
            ]
        | (y, row) <- zip [0..] maze
        ]

-- Handle movement key presses
handleMove :: Maze -> Char -> Maze
handleMove maze key =
    let oldPos = getPos Player maze
        newPos = tryNewPos key oldPos
    in if isValidPos maze newPos
        then movePlayer oldPos newPos maze
        else maze

-- Get position of a tile in the maze
getPos :: Tile -> Maze -> Pos
getPos tile maze =
    head
        [ (y, x)
        | (y, row) <- zip [0..] maze
        , (x, t)   <- zip [0..] row
        , t == tile
        ]

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
movePlayer oldPos newPos =
    replaceAt newPos Player . replaceAt oldPos Empty

-- Replace a tile in maze
replaceAt :: Pos -> Tile -> Maze -> Maze
replaceAt (y, x) newTile maze =
    [ [ if rowIdx == y && colIdx == x then newTile else tile
      | (colIdx, tile) <- zip [0..] row
      ]
    | (rowIdx, row) <- zip [0..] maze
    ]

-- Run when win
handleWin :: GameState -> IO ()
handleWin gs =
    getCurrentTime >>= \endTime ->
        let time  = calcTimelapse (gsStartTime gs) endTime
            score = Score (gsName gs) (gsDiff gs) time
        in
            putStrLn "You escaped!" >>
            putStrLn ("Time taken: " ++ show time ++ " seconds") >>
            writeScore scoresFile score

-- Calculate timelapse
calcTimelapse :: UTCTime -> UTCTime -> Double
calcTimelapse start end =
    realToFrac $ diffUTCTime end start

-- Game loop
loop :: GameState -> IO ()
loop gs =
    getChar >>= \key ->
        clearScreen >>
        let newMaze = handleMove (gsMaze gs) key
            newPos  = getPos Player newMaze
            newGS = gs
                    { gsMaze = newMaze
                    , gsPos = newPos
                    }
        in printMaze newGS >>

        if Exit `notElem` concat newMaze
            then handleWin newGS
            else loop newGS

playGame :: Name -> Difficulty -> IO ()
playGame name diff =
    hSetBuffering stdin NoBuffering >>      -- Disable buffering (Key read immediately)
    hSetEcho stdin False >>     -- Don't print key entered

    clearScreen >>
    getCurrentTime >>= \startTime ->
        let initialGS = GameState
                { gsMaze = initialMaze
                , gsName = name
                , gsDiff = diff
                , gsStartTime = startTime
                , gsPos = getPos Player initialMaze
                }
        in
            printMaze initialGS >>
            loop initialGS >>

    -- Restore terminal state
    hSetBuffering stdin LineBuffering >>
    hSetEcho stdin True
