{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp ( run )
import Html

app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseBuilder
        status200
        [ ("Content-Type", "text/html") ]
        allHtml

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app

