module Style where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Clay hiding (blue, grey)

css :: Css
css = do
    "*" ? userSelect none
    h1 <> h2 <> h3 ? fontWeight (FontWeight $ Value "400")
    header ? do
        fontSize (pct 75)
        h1 ? marginAll nil
        position relative
        padding2 (em 1) (em 1.5)
        boxShadow . pure $ bsColor (rgba 0 0 0 0.075) $ shadowWithBlur nil (em 0.125) (em 0.25)
        ".theme" ? do
            float floatRight
            borderRadiusAll (em 0.2)
            paddingAll (em 0.5)
            hover & backgroundColor (setA 0.1 grey)
    main_ |> ".list-detail" ? do
        ".list" ? do
            borderRight (em 0.1) solid (rgba 0 0 0 0.1)
            ul ? do
                listStyleType none
                paddingAll nil
                marginAll nil
                marginTop (em 1)
                li ? do
                    borderLeft (em 0.2) solid transparent
                    ".selected" & do
                        fontWeight (FontWeight $ Value "500")
                        borderLeftColor blue
                a ? do
                    display block
                    lineHeight (em 2)
                    paddingLeft (em 1)
                    minWidth (em 10)
                    hover & backgroundColor (setA 0.1 grey)
        self |> ".detail" ? do
            paddingAll (em 2)
    ".tabs" |> ".detail" ? do
        paddingTop (em 1)
    ".grid" |> ".cell" ? do
        display inlineBlock
        fontSize (em 2)
        lineHeight (em 2)
        paddingAll (em 0.5)
        marginAll (em 0.1)
        borderRadiusAll (em 0.2)
        cursor pointer
        hover & backgroundColor (setA 0.1 grey)
    form ? do
        maxWidth (em 30)
        margin2 nil auto
