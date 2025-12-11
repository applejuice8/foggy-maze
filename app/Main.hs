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

-- putStrLn for each list item
printMaze :: Maze -> IO ()
printMaze = mapM_ print

-- Find col in a row recursively
getColIndex :: Char -> String -> Int -> Int
getColIndex _ [] _ = error "Symbol not found in row"
getColIndex symbol (col:cols) x
    | col == symbol = x
    | otherwise = getColIndex symbol cols (x + 1)

-- Find row in maze recursively
getRowIndex :: Char -> [String] -> Int -> Int
getRowIndex _ [] _ = error "Symbol not found in maze"
getRowIndex symbol (row:rows) y
    | symbol `elem` row = y
    | otherwise = getRowIndex symbol rows (y + 1)

-- Combine (row, col)
getIndex :: Char -> [String] -> Pos
getIndex symbol maze =
        (row, col)
    where
        row = getRowIndex symbol maze 0
        col = getColIndex symbol (maze !! row) 0

-- Top, Left, Bottom, Right
handleKey :: Char -> Pos -> Pos
handleKey key (y, x) = 
    case key of
        'W' -> (y - 1, x)
        'A' -> (y, x - 1)
        'S' -> (y + 1, x)
        'D' -> (y, x + 1)
        _ -> (y, x)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering     -- Disable buffering (Key read immediately)
    hSetEcho stdin False    -- Don't print key entered

    putStrLn "Press keys (q to quit):"
    loop initialMaze

loop :: Maze -> IO ()
loop maze = do
    key <- getChar
    let index = getIndex '&' maze
        newIndex = handleKey key index

    putStrLn $ "You pressed: " ++ [key]

    printMaze maze
    if c == 'q'
        then putStrLn "Exiting..."
        else loop maze
