module Arc.Clay.Inputs.Text where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Arc.Widgets.Text (TextInputSize (..))
import Clay
import Prelude hiding (rem)

textInputStyle :: Css
textInputStyle = do
    background transparent
    color inherit
    border (rem 0.12) solid inputBorder
    borderRadiusAll (rem 0.3)
    padding2 (em 0) (rem 0.5)
    transition "border" (ms 100) easeInOut (sec 0)
    focus & borderColor inputSelectedBorder
    class_ TextInputFull & do
        width $ pct 100
        height $ rem 2.8
        lineHeight $ rem 2.8
    class_ TextInputInline & do
        width $ rem 10
        height $ rem 2
        margin2 (rem 0) (rem 0.3)

emailInputStyle :: Css
emailInputStyle = textInputStyle

passwordInputStyle :: Css
passwordInputStyle = do
    textInputStyle
