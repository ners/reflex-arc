module Arc.Clay.Form where

import Arc.Clay.Util
import Clay

forms :: Css
forms = form ? formStyle

formStyle :: Css
formStyle = do
    ".form-field" <> fieldset <> ".buttons" ? do
        margin2 (em 1) 0
    ".buttons" ? do
        button ? float floatRight
        marginTop (em 2)
