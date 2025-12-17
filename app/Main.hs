module Main where

import System.IO
import Data.Char (toUpper)
import System.Console.ANSI (clearScreen)
import Data.Time.Clock

import ScoreManager
import Game

main :: IO ()
main = do
    playGame
