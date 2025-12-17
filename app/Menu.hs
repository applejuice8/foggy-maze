import Text.Read (readMaybe)

-- Show menu
menu :: IO ()
menu = 
    putStrLn "===============================" <>
    putStrLn "= 1. Play                     =" <>
    putStrLn "= 2. View top 5 scores        =" <>
    putStrLn "= 3. Exit                     =" <>
    putStrLn "===============================" <>
    putStrLn "Enter your input ="

-- Rectangle area calculation
calculateRectangle :: Num a => a -> a -> a
calculateRectangle width height = width * height

-- Prompt name
promptName :: IO Int
promptName = do
    putStrLn "Enter name: "
    input <- getLine
    case readMaybe input of
        Just name -> return name
        Nothing -> do
            putStrLn "Invalid name. Please try again."
            promptName

-- Process menu selection
process :: String -> IO ()
process choice = case choice of
    "1" -> do
        name <- promptName
        putStrLn name
        main
    "2" -> putStrLn "Print top scores"
    "3" -> putStrLn "Thanks for playing!"
    _   -> do
        putStrLn "Invalid selection, try again."
        main

-- Main function
main :: IO ()
main = do
    menu
    choice <- getLine
    process choice
