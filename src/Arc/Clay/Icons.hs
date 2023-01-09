{-# LANGUAGE FlexibleContexts #-}

module Arc.Clay.Icons where

import Arc.Clay.Util
import Arc.Tokens.Size (SizeToken (..))
import Clay
import Clay.Stylesheet (key)

icons :: Css
icons = ".icon" ? iconStyle

iconStyle :: Css
iconStyle = do
    display inlineBlock
    position relative
    margin2 (em 0) (em 0.2)
    lineHeight (em 1)
    mdiFont
    class_ MediumSize & do
        fontSize (pct 150)
    class_ LargeSize & do
        fontSize (pct 200)
    img <> svg ? do
        squareSize (em 1)
        position relative
        top $ em 0.1
        "path" ? do
            key "fill" $ Value "currentColor"

iconWithTexts :: Css
iconWithTexts = do
    ".icon-with-text" ? iconWithTextStyle

iconWithTextStyle :: Css
iconWithTextStyle = do
    display inlineBlock
    self |> ".text" ? display inlineBlock
    self |> ".icon" ? do
        position relative
        class_ SmallSize & do
            bottom nil
        class_ MediumSize & do
            bottom (em (-0.15))
        class_ LargeSize & do
            bottom (em (-0.2))
