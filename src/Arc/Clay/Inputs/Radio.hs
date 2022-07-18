module Arc.Clay.Inputs.Radio where

import Arc.Clay.Colours
import Arc.Clay.Inputs.Checkbox
import Arc.Clay.Util
import Clay
import qualified Data.Text as Text
import Web.Font.MDI

radioInputStyle :: Css
radioInputStyle = do
    checkboxStyle
    before & do
        content $ stringContent $ Text.singleton mdiRadioboxBlank
    checked & before & do
        content $ stringContent $ Text.singleton mdiCheckboxMarkedCircleOutline

radioInputGroupStyle :: Css
radioInputGroupStyle = checkboxInputGroupStyle
