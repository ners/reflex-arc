module Arc.Clay.Inputs where

import Arc.Clay.Inputs.Checkbox
import Arc.Clay.Inputs.Radio
import Arc.Clay.Inputs.Text
import Arc.Clay.Util
import Clay

inputs :: Css
inputs = do
    input ? inputStyle
    ".input-group" ? inputGroupStyle

inputStyle :: Css
inputStyle = do
    "type" @= "text" & textInputStyle
    "type" @= "password" & textInputStyle
    "type" @= "radio" & radioInputStyle
    "type" @= "checkbox" & checkboxStyle

inputGroupStyle :: Css
inputGroupStyle = do
    position relative
    margin2 (em 0.5) nil
    ".radio" & radioInputGroupStyle
    ".checkbox" & radioInputGroupStyle
