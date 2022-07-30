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
    verticalAlign middle
    mdiFont
    class_ SmallSize & do
        svg ? squareSize (em 1)
    class_ MediumSize & do
        svg ? squareSize (em 1.5)
        fontSize (pct 150)
    class_ LargeSize & do
        svg ? squareSize (em 2)
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
        class_ LargeSize & self |~ ".text" ? do
            height largeSize
            lineHeight largeSize
