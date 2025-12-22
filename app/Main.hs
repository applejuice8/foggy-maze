{-# LANGUAGE LambdaCase #-}     -- For lambda case

module Main where

import Text.Read (readMaybe)
import Config (scoresFile)
import Game (playGame)
import ScoreManager (Name, Difficulty(..), readScores, topScores)

-- Data types conversion
intToDiff :: Int -> Difficulty
intToDiff = \case
    1 -> Easy
    2 -> Medium
    3 -> Hard
    4 -> Insane
    _ -> Easy   -- Default

-- Show menu
menu :: IO ()
menu = 
    putStrLn "\n================================" <>
    putStrLn "|    Maze Game - Fog of War    |" <>
    putStrLn "================================" <>
    putStrLn "| 1. Play game                 |" <>
    putStrLn "| 2. View top scores           |" <>
    putStrLn "| 3. How to play?              |" <>
    putStrLn "| 4. Reset scores              |" <>
    putStrLn "| 5. Exit                      |" <>
    putStrLn "================================" <>
    putStrLn "Enter your choice: "

-- Prompt inputs
promptName :: IO Name
promptName =
    putStrLn "Enter name: " >>
    getLine >>= \name ->
        if null name
            then
                putStrLn "Invalid name. Please try again." >>
                promptName
            else return name

promptDiff :: IO Difficulty
promptDiff = 
    putStrLn "\nSelect difficulty: " <>
    putStrLn "1. Easy (9x9 tiles)" <>
    putStrLn "2. Medium (7x7 tiles)" <>
    putStrLn "3. Hard (5x5 tiles)" <>
    putStrLn "4. Insane (3x3 tiles)" <>
    putStrLn "Your choice: " >>
    getLine >>= \input ->
        case readMaybe input :: Maybe Int of
            Just 1 -> return Easy
            Just 2 -> return Medium
            Just 3 -> return Hard
            Just 4 -> return Insane
            _      ->
                putStrLn "Invalid choice. Please enter 1, 2, 3 or 4." >>
                promptDiff

promptN :: IO Int
promptN =
    putStrLn "How many top scores do you want?" >>
    getLine >>= \input ->
        case readMaybe input :: Maybe Int of
            Just n  -> return n
            Nothing ->
                putStrLn "Invalid choice. Please enter an integer." >>
                promptN

-- Process menu selection
process :: String -> IO ()
process choice = case choice of
    "1" ->
        promptName >>= \name ->
            promptDiff >>= \diff ->
                playGame name diff >>
        main

    "2" ->
        readScores scoresFile >>= \scores ->
            promptN >>= \n ->
                putStrLn ("\n========= Top " ++ show n ++ " Scores =========") >>
                mapM_ print (topScores n scores) >>
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
