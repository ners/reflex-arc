module Main where

import Arc.Clay.Fonts
import Arc.Clay.Normalise
import Arc.Clay.Util
import Arc.Main
import Clay
import qualified Data.Text.IO as Text
import Page

main :: IO ()
main = do
    Text.putStrLn $ renderText css
    arcMainWithCss css page

css :: Css
css = do
    header ? do
        color $ rgba 0 0 0 0.75
        position relative
        paddingAll (em 1)
        h1 <> h2 <> h3 ? do
            marginAll nil
            fontWeight normal
        after & do
            height (em 0.3)
            position absolute
            top (pct 100)
            left nil
            right nil
            background [vGradient transparent (rgba 0 0 0 0.05)]
            content $ stringContent ""
    main_ ? paddingAll (em 2)

--rgba(0, 0, 0, 0) linear-gradient(rgba(9, 30, 66, 0.13) 0px, rgba(9, 30, 66, 0.13) 1px, rgba(9, 30, 66, 0.08) 1px, rgba(9, 30, 66, 0) 4px) repeat scroll 0% 0%
