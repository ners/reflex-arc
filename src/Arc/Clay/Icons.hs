{-# LANGUAGE FlexibleContexts #-}

module Arc.Clay.Icons where

import Arc.Clay.Util
import Arc.Tokens.Size (SizeToken (..))
import Arc.Widgets.Icon (Icon (..))
import Clay
import Clay.Stylesheet (key)

icons :: Css
icons = baseClass_ @Icon ? iconStyle

iconStyle :: Css
iconStyle = do
    display inlineBlock
    position relative
    margin2 (em 0) (em 0.3)
    mdiFont
    class_ SmallSize & do
        (self <> svg) ? squareSize (em 1)
        top (em 0.125)
    class_ MediumSize & do
        (self <> svg) ? squareSize (em 1.5)
        top (em 0.3)
        fontSize (pct 150)
    class_ LargeSize & do
        (self <> svg) ? squareSize (em 2)
        top (em 0.55)
        fontSize (pct 200)
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
    self |> ".text" ? display inlineBlock
    self |> baseClass_ @Icon ? do
        marginLeft nil
        position relative
        class_ SmallSize & self |~ ".text" ? do
            height smallSize
            lineHeight smallSize
        class_ MediumSize & self |~ ".text" ? do
            height mediumSize
            lineHeight mediumSize
            top (em 0.15)
        class_ LargeSize & self |~ ".text" ? do
            height largeSize
            lineHeight largeSize
            top (em 0.15)
