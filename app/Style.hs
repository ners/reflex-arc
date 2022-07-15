module Style where

import Arc.Clay.Fonts
import Arc.Clay.Normalise
import Arc.Clay.Util
import Clay
import Clay.Font

css :: Css
css = do
    h1 <> h2 <> h3 ? fontWeight (FontWeight $ Value "500")
    header ? do
        fontSize (pct 75)
        h1 ? marginAll nil
        color $ rgba 0 0 0 0.75
        position relative
        padding2 (em 1) (em 1.5)
        boxShadow . pure $ bsColor (rgba 0 0 0 0.075) $ shadowWithBlur nil (em 0.125) (em 0.25)
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
                    ".selected" & borderColor (rgba 0 0 125 0.7)
                a ? do
                    display block
                    lineHeight (em 2)
                    paddingLeft (em 1)
                    minWidth (em 10)
                    cursor pointer
                    hover & backgroundColor (rgba 0 0 0 0.05)
        ".detail" ? do
            paddingAll (em 2)
    form ? do
        maxWidth (em 30)
        margin2 nil auto
