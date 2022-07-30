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
    ".medium" & fontSize (pct 150)
    ".large" & fontSize (pct 200)
    svg ? do
        squareSize (em 1)
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
    let smallSize = em 1
    let mediumSize = em 1.5
    let largeSize = em 2
    self |> ".icon" ? do
        marginLeft nil
        position relative
        ".medium" & top (em 0.15)
        ".large" & top (em 0.15)
    --self |> svg ? position absolute
    self |> ".text" ? display inlineBlock
