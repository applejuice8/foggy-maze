module Main where

import System.IO
import System.Console.ANSI
import Control.Monad (when)

-- Types
type Maze = [String]
type Pos = (Int, Int)   -- (x, y)

-- Sample Maze
maze :: Maze
maze =
    [ "#########"
    , "#       #"
    , "# ### # #"
    , "# #   # #"
    , "# # ### #"
    , "#       #"
    , "#########"
    ]

-- Render the maze with player
renderMaze :: Maze -> Pos -> IO ()
renderMaze mz playerPos = do
    mapM_ putStrLn $ mapWithCoords mz playerPos

-- Replace character at player's position
mapWithCoords :: Maze -> Pos -> [String]
mapWithCoords mz (px, py) =
    [ [if (x, y) == (px, py) then '@' else c
    | (x, c) <- zip [0..] row]
    | (y, row) <- zip [0..] mz
    ]

-- Check if a position is walkable
isValid :: Maze -> Pos -> Bool
isValid mz (x, y) = (mz !! y !! x) /= '#'

-- Read a single character (non-blocking)
readKey :: IO Char
readKey = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    getChar

-- Calculate new position based on input
movePlayer :: Pos -> Char -> Pos
movePlayer (x, y) c = case c of
    'w' -> (x, y - 1)
    's' -> (x, y + 1)
    'a' -> (x - 1, y)
    'd' -> (x + 1, y)
    _   -> (x, y)  -- Invalid key

-- Game loop
gameLoop :: Maze -> Pos -> IO ()
gameLoop mz pos = do
    renderMaze mz pos
    c <- readKey
    let newPos = movePlayer pos c
    when (c /= 'q') $ gameLoop mz (if isValid mz newPos then newPos else pos)

-- Entry point
main :: IO ()
main = do
    let startPos = (1, 1)
    gameLoop maze startPos
