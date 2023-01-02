module Arc.Clay.Inputs.Radio where

import Arc.Clay.Inputs.Checkbox
import Arc.Clay.Util
import Clay
import Web.Font.MDI

radioInputStyle :: Css
radioInputStyle = do
    checkboxStyle
    before & do
        content $ charContent $ mdiChar MdiRadioboxBlank
    checked & before & do
        content $ charContent $ mdiChar MdiCheckboxMarkedCircleOutline

radioInputGroupStyle :: Css
radioInputGroupStyle = checkboxInputGroupStyle
