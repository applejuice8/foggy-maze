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

printMaze :: Maze -> IO ()
printMaze = mapM_ print

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering     -- Disable buffering (Key read immediately)
    hSetEcho stdin False    -- Don't print key entered

    putStrLn "Press keys (q to quit):"
    loop initialMaze

loop :: Maze -> IO ()
loop maze = do
    c <- getChar
    putStrLn $ "You pressed: " ++ [c]
    printMaze maze
    if c == 'q'
        then putStrLn "Exiting..."
        else loop maze
