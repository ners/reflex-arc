module Main where

import Arc.Main
import Page
import Style

main :: IO ()
main = arcMainWithCss css page
