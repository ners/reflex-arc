{-# LANGUAGE FlexibleContexts #-}

module Arc.Clay.Icons where

import Arc.Clay.Util
import Clay
import Clay.Stylesheet (key)

icons :: Css
icons = ".icon" ? iconStyle

iconStyle :: Css
iconStyle = do
    display inlineBlock
    position relative
    margin2 (em 0) (em 0.3)
    ".small" & do
        (self <> svg) ? squareSize (em 1)
        top (em 0.125)
    ".medium" & do
        (self <> svg) ? squareSize (em 1.5)
        top (em 0.3)
    ".large" & do
        (self <> svg) ? squareSize (em 2)
        top (em 0.55)
    svg ? do
        left nil
        top nil
        position absolute
        "path" ? do
            key "fill" $ Value "currentColor"
