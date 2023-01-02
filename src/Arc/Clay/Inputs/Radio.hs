module Arc.Clay.Inputs.Radio where

import Arc.Clay.Inputs.Checkbox
import Arc.Clay.Util
import Clay
import Web.Font.MDI

radioInputStyle :: Css
radioInputStyle = do
    checkboxStyle
    before & do
        content $ charContent mdiRadioboxBlank
    checked & before & do
        content $ charContent mdiCheckboxMarkedCircleOutline

radioInputGroupStyle :: Css
radioInputGroupStyle = checkboxInputGroupStyle
