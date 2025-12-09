{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Web.Scotty

greet :: Web.Scotty.ActionM ()
greet = Web.Scotty.html "<h1>Hello, adam</h1>"

main :: IO ()
main = do
    Web.Scotty.scotty 3000 $ do
        Web.Scotty.get "/" $ do
            greet
