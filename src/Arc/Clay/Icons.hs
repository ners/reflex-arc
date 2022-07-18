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
    mdiFont
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

iconWithTexts :: Css
iconWithTexts = do
    ".icon-with-text" ? iconWithTextStyle
    ".icon-with-text" |+ star ? marginLeft (em 0.25)
    star |+ ".icon-with-text" ? marginLeft (em 0.25)

iconWithTextStyle :: Css
iconWithTextStyle = do
    display inlineBlock
    position relative
    let smallSize = em 1
    let mediumSize = em 1.5
    let largeSize = em 2
    self |> ".icon" ? do
        marginLeft nil
        ".small" & self |~ ".text" ? do
            height smallSize
            lineHeight smallSize
        ".medium" & self |~ ".text" ? do
            height mediumSize
            lineHeight mediumSize
        ".large" & self |~ ".text" ? do
            height largeSize
            lineHeight largeSize
    self |> ".text" ? display inlineBlock
