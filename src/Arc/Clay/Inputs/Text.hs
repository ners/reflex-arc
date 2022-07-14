module Arc.Clay.Inputs.Text where

import Arc.Clay.Colours
import Arc.Clay.Util
import Clay

textInputStyle :: Css
textInputStyle = do
    border (em 0.125) solid (rgb 223 225 230)
    borderRadiusAll (em 0.3)
    padding2 (em 0) (em 0.3)
    transition "border" (ms 100) easeInOut (sec 0)
    focus & borderColor (rgb 76 154 255)
    ".TextInputFull" & do
        width $ pct 100
        height $ em 2.5
        lineHeight $ em 2.5
    ".TextInputInline" & do
        width $ em 10
        margin2 (em 0) (em 0.3)

emailInputStyle :: Css
emailInputStyle = textInputStyle

passwordInputStyle :: Css
passwordInputStyle = textInputStyle
