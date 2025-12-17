module Main where

import Game (Name, playGame)
import ScoreManager (readScores, topScores)

-- Show menu
menu :: IO ()
menu = 
    putStrLn "\n================================" <>
    putStrLn "| 1. Play game                 |" <>
    putStrLn "| 2. View top 5 scores         |" <>
    putStrLn "| 3. How to play?              |" <>
    putStrLn "| 4. Reset scores              |" <>
    putStrLn "| 5. Exit                      |" <>
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

    "3" -> do
        putStrLn "\n========= How to Play? ========="
        putStrLn "- Find the exit (E) in the maze"
        putStrLn "- Use the WASD keys to move"
        putStrLn "- Can only see 5x5 area around player"
        putStrLn "- Score is based on time taken"
        main

    "4" -> do
        putStrLn "Are you sure? (y/n): "
        confirm <- getLine
        if confirm == "y"
            then do
                writeFile "app/scores.csv" ""
                putStrLn "Scores reset!"
            else putStrLn "Cancelled"
        main

    "5" -> putStrLn "Thanks for playing!"

    _   -> do
        putStrLn "Invalid choice. Please try again."
        main

-- Main function
main :: IO ()
main = do
    menu
    choice <- getLine
    process choice
