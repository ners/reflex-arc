module Main where

import Arc.Clay.App (appStyle)
import Arc.Clay.Util (renderText)
import Arc.Main

import qualified Data.Text.IO as Text
import Page

main :: IO ()
main = do
    Text.putStrLn $ renderText appStyle
    arcMain page
