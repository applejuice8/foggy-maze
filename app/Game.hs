module Game (startGame) where

import System.IO (hSetBuffering, hSetEcho, stdin, BufferMode(..))
import Data.Char (toUpper)
import System.Console.ANSI (clearScreen)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Config (scoresFile)
import Types
import Convert
import ScoreManager (writeScore)

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

    let maze     = gsMaze gs
        (py, px) = gsPos gs
        size     = diffToSize $ gsDiff gs

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

-- Get position of a tile in the maze
getPos :: Tile -> Maze -> Pos
getPos tile maze =
    head
        [ (y, x)
        | (y, row) <- zip [0..] maze
        , (x, t)   <- zip [0..] row
        , t == tile
        ]

-- Handle movement key presses
handleMove :: GameState -> Char -> GameState
handleMove gs key =
    case charToDirection (toUpper key) of
        Nothing -> gs
        Just dir
                | isValidPos maze newPos ->
                    gs {gsMaze = newMaze, gsPos = newPos}
                | otherwise -> gs
            where
                maze    = gsMaze gs
                oldPos  = gsPos gs
                newPos  = move dir oldPos
                newMaze = movePlayer oldPos newPos maze

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
            putStrLn (colorCode Green ++ "You escaped!" ++ colorCode Reset) >>
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
        let newGS = handleMove gs key
            newMaze = gsMaze newGS
        in printMaze newGS >>

        if Exit `notElem` concat newMaze
            then handleWin newGS
            else loop newGS

-- Main function
startGame :: Name -> Difficulty -> IO ()
startGame name diff =
    hSetBuffering stdin NoBuffering >>      -- Disable buffering (Key read immediately)
    hSetEcho stdin False >>     -- Don't print key entered

    clearScreen >>
    getCurrentTime >>= \startTime ->
        let initialGS = GameState
                { gsMaze      = initialMaze
                , gsName      = name
                , gsDiff      = diff
                , gsStartTime = startTime
                , gsPos       = getPos Player initialMaze
                }
        in
            printMaze initialGS >>
            loop initialGS >>

    -- Restore terminal state
    hSetBuffering stdin LineBuffering >>
    hSetEcho stdin True
