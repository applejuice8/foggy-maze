module Main where

import System.IO

-- Types
type Maze = [String]
type Pos = (Int, Int)   -- (x, y)

-- Sample Maze
initialMaze :: Maze
initialMaze =
    [ "#########"
    , "#       #"
    , "# ### # #"
    , "# #   # #"
    , "# # ### #"
    , "#       #"
    , "#########"
    ]

main :: IO ()
main = do
    -- Disable buffering and echo so keys are read immediately
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    putStrLn "Press keys (q to quit):"
    loop

loop :: IO ()
loop = do
    c <- getChar          -- read a single character immediately
    putStrLn $ "You pressed: " ++ [c]
    if c == 'q'
        then putStrLn "Exiting..."
        else loop

