import Text.Read (readMaybe)

-- Show menu
menu :: IO ()
menu = do
    putStrLn "==============================="
    putStrLn "= 1. Calculate Area Rectangle ="
    putStrLn "= 2. Exit ="
    putStrLn "==============================="
    putStrLn "Enter your input ="

-- Rectangle area calculation
calculateRectangle :: Num a => a -> a -> a
calculateRectangle width height = width * height

-- Prompt for width
promptWidth :: IO Int
promptWidth = do
    putStrLn "Enter width ="
    input <- getLine
    case readMaybe input of
        Just n -> return n
        Nothing -> do
            putStrLn "Invalid number, try again."
            promptWidth

-- Prompt for height
promptHeight :: IO Int
promptHeight = do
    putStrLn "Enter height ="
    input <- getLine
    case readMaybe input of
        Just n -> return n
        Nothing -> do
            putStrLn "Invalid number, try again."
            promptHeight

-- Process menu selection
process :: String -> IO ()
process choice = case choice of
    "1" -> do
        width <- promptWidth
        height <- promptHeight
        putStrLn ("The result is " <> show (calculateRectangle width height))
        main
    "2" -> putStrLn "Thanks for using the program"
    _   -> do
        putStrLn "Invalid selection, try again."
        main

-- Main function
main :: IO ()
main = do
    menu
    choice <- getLine
    process choice
