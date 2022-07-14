module Arc.Clay.Inputs.Radio where

import Arc.Clay.Colours
import Arc.Clay.Inputs.Checkbox
import Arc.Clay.Util
import Clay

radioInputStyle :: Css
radioInputStyle = do
    checkboxStyle
    borderRadiusAll $ pct 50

radioInputGroupStyle :: Css
radioInputGroupStyle = checkboxInputGroupStyle
