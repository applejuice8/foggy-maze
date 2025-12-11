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

-- Find column of a character in a row recursively
getColIndex :: Char -> String -> Int -> Maybe Int
getColIndex _ [] _ = Nothing
getColIndex symbol (col:cols) x
    | col == symbol = Just x
    | otherwise = getColIndex symbol cols (x + 1)

-- Find the (row, col) in the 2D list, recursively
getRowIndex :: Char -> [String] -> Int -> (Int, Int)
getRowIndex _ [] _ = error "Symbol not found"
getRowIndex symbol (row:rows) y =
    case getColIndex symbol row 0 of
        Just col -> (y, col)
        Nothing  -> getRowIndex symbol rows (y + 1)


handleKey :: Char -> IO ()
handleKey c = 
    case c of
        'W' -> 

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering     -- Disable buffering (Key read immediately)
    hSetEcho stdin False    -- Don't print key entered

    putStrLn "Press keys (q to quit):"
    loop initialMaze

loop :: Maze -> IO ()
loop maze = do
    c <- getChar
    handleKey c
    putStrLn $ "You pressed: " ++ [c]
    printMaze maze
    if c == 'q'
        then putStrLn "Exiting..."
        else loop maze
