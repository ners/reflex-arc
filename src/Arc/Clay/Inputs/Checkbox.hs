module Arc.Clay.Inputs.Checkbox where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Clay
import Web.Font.MDI

checkboxStyle :: Css
checkboxStyle = do
    width $ em 2
    borderStyle none
    color inputBorder
    before & do
        mdiFont
        fontSize (pct 130)
        transition "color" (ms 100) easeInOut (sec 0)
        content $ charContent $ mdiChar MdiCheckboxBlankOutline
    checked & before & do
        color inputSelectedBorder
        content $ charContent $ mdiChar MdiCheckboxMarkedOutline
    (self <> sibling label) ? do
        transition "opacity" (ms 100) easeInOut (sec 0)
        cursor pointer
    disabled
        & (self <> sibling label)
            ? do
                cursor notAllowed
                opacity 0.5

checkboxInputGroupStyle :: Css
checkboxInputGroupStyle = do
    paddingLeft (em 2.25)
    self |> input ? do
        position absolute
        top (em 0)
        left (em 0.25)
        marginAll nil
