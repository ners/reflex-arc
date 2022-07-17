module Arc.Clay.Inputs.Radio where

import Arc.Clay.Inputs.Checkbox
import Arc.Clay.Util
import Arc.Tokens.Colour
import Clay

radioInputStyle :: Css
radioInputStyle = do
    checkboxStyle
    borderRadiusAll $ pct 50

radioInputGroupStyle :: Css
radioInputGroupStyle = checkboxInputGroupStyle
