
module Main where

import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering   -- read input immediately
    hSetEcho stdin False              -- optional: do not display typed keys
    loop

loop :: IO ()
loop = do
    c <- getChar                     -- reads a single key immediately
    putStrLn $ "You pressed: " ++ [c]
    if c /= 'q' then loop else return ()
