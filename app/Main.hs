module Main where

import Text.Read (readMaybe)
import Text.Printf (printf)
import Config (scoresFile)
import Types (Name, Difficulty(..), Score(..))
import Game (startGame)
import ScoreManager (readScores, topN)

-- Show menu
menu :: IO ()
menu = 
    putStrLn "\n================================" <>
    putStrLn "|       Foggy Maze Game        |"   <>
    putStrLn "================================"   <>
    putStrLn "| 1. Play game                 |"   <>
    putStrLn "| 2. View top scores           |"   <>
    putStrLn "| 3. How to play?              |"   <>
    putStrLn "| 4. Reset scores              |"   <>
    putStrLn "| 5. Exit                      |"   <>
    putStrLn "================================"   <>
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
    putStrLn "1. Easy (9x9 tiles)"   <>
    putStrLn "2. Medium (7x7 tiles)" <>
    putStrLn "3. Hard (5x5 tiles)"   <>
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
            Just n | n > 0 -> return n
            _              ->
                            putStrLn "Invalid choice. Please enter an integer greater than 0." >>
                            promptN

-- Format score
formatScore :: (Int, Score) -> String
formatScore (i, score) = 
    printf "%2d | %-10s | %-7s | %.2fs"
        i
        (playerName score)
        (show $ difficulty score)
        (timeTaken score)

-- Option 1 (Play game)
playGame :: IO ()
playGame =
    promptName >>= \name ->
        promptDiff >>= \diff ->
            startGame name diff >>
    main

-- Option 2 (View top scores)
topScores :: IO ()
topScores =
    readScores scoresFile >>= \scores ->
    promptN >>= \n ->
        putStrLn ("\n========= Top " ++ show n ++ " Scores =========") >>
        let scoreList = topN n scores
        in mapM_ (putStrLn . formatScore) (zip [1..] scoreList) >>
    main

-- Option 3 (How to play?)
gameInstructions :: IO ()
gameInstructions =
    putStrLn "\n=============== How to Play? ===============" <>
    putStrLn "- Your location is denoted by P"                <>
    putStrLn "- Hash symbols (#) are walls"                   <>
    putStrLn "- Question marks (?) are fogs"                  <>
    putStrLn "- Find the exit (E) in the maze"                <>
    putStrLn "- Use the WASD keys to move"                    <>
    putStrLn "- Can only see small area around player"        <>
    putStrLn "- Score is based on time taken and difficulty"  <>
    main

-- Option 4 (Reset scores)
resetScores :: IO ()
resetScores =
    putStrLn "Are you sure? (y/n): " >>
    getLine >>= \confirm ->
        (if confirm == "y"
            then
                writeFile scoresFile "" >>
                putStrLn "Scores reset!"
            else putStrLn "Cancelled"
        ) >>
    main

-- Process menu selection
process :: String -> IO ()
process choice = case choice of
    "1" -> playGame
    "2" -> topScores
    "3" -> gameInstructions
    "4" -> resetScores
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
