module Main where

import Game (Name, playGame)
import ScoreManager (readScores, topScores)

-- Show menu
menu :: IO ()
menu = 
    putStrLn "================================" <>
    putStrLn "| 1. Play                      |" <>
    putStrLn "| 2. View top 5 scores         |" <>
    putStrLn "| 3. Exit                      |" <>
    putStrLn "================================" <>
    putStrLn "Enter your choice: "

-- Prompt name
promptName :: IO Name
promptName = do
    putStrLn "Enter name: "
    name <- getLine
    if null name
        then do
            putStrLn "Invalid name. Please try again."
            promptName
        else return name

-- Process menu selection
process :: String -> IO ()
process choice = case choice of
    "1" -> do
        name <- promptName
        playGame name
        main

    "2" -> do
        let file = "app/scores.csv"
        scores <- readScores file
        putStrLn "\n========= Top 5 Scores ========="
        mapM_ print (topScores 5 scores)
        main

    "3" -> putStrLn "Thanks for playing!"

    _   -> do
        putStrLn "Invalid choice. Please try again."
        main

-- Main function
main :: IO ()
main = do
    menu
    choice <- getLine
    process choice
