module Arc.Clay.Inputs.Checkbox where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Clay hiding (blue)
import qualified Data.Text as Text
import Web.Font.MDI

checkboxStyle :: Css
checkboxStyle = do
    borderStyle none
    color uncheckedColour
    before & do
        mdiFont
        fontSize (pct 130)
        transition "color" (ms 100) easeInOut (sec 0)
        content $ charContent mdiCheckboxBlankOutline
    checked & before & do
        color checkedColour
        content $ charContent mdiCheckboxMarkedOutline
    (self <> sibling label) ? do
        transition "opacity" (ms 100) easeInOut (sec 0)
        cursor pointer
    disabled & (self <> sibling label) ? do
        cursor notAllowed
        opacity 0.5
  where
    uncheckedColour = rgb 223 225 230
    borderUnchecked = rgb 223 225 230
    checkedColour = blue
    borderChecked = blue

checkboxInputGroupStyle :: Css
checkboxInputGroupStyle = do
    paddingLeft (em 2.25)
    self |> input ? do
        position absolute
        top (em 0)
        left (em 0.25)
        marginAll nil
