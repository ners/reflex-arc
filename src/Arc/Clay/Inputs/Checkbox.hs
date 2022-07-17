module Arc.Clay.Inputs.Checkbox where

import Arc.Tokens.Colour
import Arc.Clay.Util
import Clay

checkboxStyle :: Css
checkboxStyle = do
    width $ em 0.8
    height $ em 0.8
    borderRadiusAll $ em 0.15
    backgroundColor $ rgba 250 251 252 1
    border (em 0.1) solid borderUnchecked
    transition "border" (ms 100) easeInOut (sec 0)
    transition "background" (ms 100) easeInOut (sec 0)
    checked & do
        border (em 0.3) solid borderChecked
    disabled & do
        backgroundColor borderUnchecked
        sibling label ? do
            cursor notAllowed
            opacity 0.5
    (self <> sibling label) ? do
        transition "opacity" (ms 100) easeInOut (sec 0)
        cursor pointer
  where
    borderUnchecked = rgba 223 225 230 1
    borderChecked = primaryColour

checkboxInputGroupStyle :: Css
checkboxInputGroupStyle = do
    paddingLeft (em 2.25)
    self |> input ? do
        position absolute
        top (em 0.4)
        left (em 0.75)
        marginAll nil
