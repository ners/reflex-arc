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
    margin2 (em 0) (em 0.3)
    ".small" & (self <> svg) ? squareSize (em 1)
    ".medium" & (self <> svg) ? squareSize (em 1.5)
    ".large" & (self <> svg) ? squareSize (em 2)
    svg ? do
        "path" ? do
            key "fill" $ Value "currentColor"
