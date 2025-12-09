{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty ( get, html, scotty, ActionM )
import Data.Text.Lazy (Text)

greet :: Text
greet = "<h1>Hello, adam</h1>"

main :: IO ()
main = scotty 3000 $ do
    get "/" $ html greet
