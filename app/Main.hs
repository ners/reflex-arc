module Main where

import Arc.Clay.Fonts
import Arc.Clay.Normalise
import Arc.Clay.Util
import Arc.Main
import Clay
import Clay.Font
import qualified Data.Text.IO as Text
import Page

main :: IO ()
main = do
    Text.putStrLn $ renderText css
    arcMainWithCss css page

css :: Css
css = do
    h1 <> h2 <> h3 ? fontWeight (FontWeight $ Value "500")
    header ? do
        fontSize (pct 50)
        h1 ? marginAll nil
        color $ rgba 0 0 0 0.75
        position relative
        padding2 (em 0.5) (em 1.5)
        after & do
            height (em 0.3)
            position absolute
            top (pct 100)
            left nil
            right nil
            background [vGradient transparent (rgba 0 0 0 0.05)]
            content $ stringContent ""
    main_ ? paddingAll (em 2)
    form ? do
        maxWidth (em 30)
        margin2 nil auto