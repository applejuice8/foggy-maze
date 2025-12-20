module Main where

import Config (scoresFile)
import Game (playGame)
import ScoreManager (Name, readScores, topScores)

-- Show menu
menu :: IO ()
menu = 
    putStrLn "\n================================" <>
    putStrLn "|    Maze Game - Fog of War    |" <>
    putStrLn "================================" <>
    putStrLn "| 1. Play game                 |" <>
    putStrLn "| 2. View top 5 scores         |" <>
    putStrLn "| 3. How to play?              |" <>
    putStrLn "| 4. Reset scores              |" <>
    putStrLn "| 5. Exit                      |" <>
    putStrLn "================================" <>
    putStrLn "Enter your choice: "

-- Prompt name
promptName :: IO Name
promptName =
    putStrLn "Enter name: " >>
    getLine >>= \name ->
        if null name
            then
                putStrLn "Invalid name. Please try again." >>
                promptName
            else return name

-- Process menu selection
process :: String -> IO ()
process choice = case choice of
    "1" ->
        promptName >>= \name ->
            playGame name >>
        main

    "2" ->
        readScores scoresFile >>= \scores ->
            putStrLn "\n========= Top 5 Scores =========" >>
            mapM_ print (topScores 5 scores) >>
        main

    "3" -> 
        putStrLn "\n========= How to Play? =========" <>
        putStrLn "- Find the exit (E) in the maze" <>
        putStrLn "- Use the WASD keys to move" <>
        putStrLn "- Can only see 5x5 area around player" <>
        putStrLn "- Score is based on time taken" <>
        main

    "4" ->
        putStrLn "Are you sure? (y/n): " >>
        getLine >>= \confirm ->
            (if confirm == "y"
                then
                    writeFile scoresFile "" >>
                    putStrLn "Scores reset!"
                else putStrLn "Cancelled"
            ) >>
        main

    "5" -> putStrLn "Thanks for playing!"

    _   ->
        putStrLn "Invalid choice. Please try again." >>
        main

-- Main function
main :: IO ()
main = 
    menu >>
    getLine >>= \choice ->
        process choice
