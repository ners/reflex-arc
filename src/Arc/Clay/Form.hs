module Arc.Clay.Form where

import Arc.Clay.Util
import Clay

forms :: Css
forms = form ? formStyle

formStyle :: Css
formStyle = do
    ".buttons" ? do
        margin2 (em 2) 0
        textAlign $ alignSide sideRight