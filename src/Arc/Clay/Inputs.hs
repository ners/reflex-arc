module Arc.Clay.Inputs where

import Arc.Clay.Inputs.Checkbox
import Arc.Clay.Inputs.Radio
import Arc.Clay.Inputs.Text
import Arc.Clay.Inputs.Textarea
import Arc.Clay.Util
import Clay

inputs :: Css
inputs = do
    input ? inputStyle
    textarea ? textareaStyle
    ".input-group" ? inputGroupStyle

inputStyle :: Css
inputStyle = do
    appearance noneValue
    "type" @= "text" & textInputStyle
    "type" @= "email" & emailInputStyle
    "type" @= "password" & passwordInputStyle
    "type" @= "radio" & radioInputStyle
    "type" @= "checkbox" & checkboxStyle

inputGroupStyle :: Css
inputGroupStyle = do
    position relative
    margin2 (em 0.5) nil
    ".checkbox" & do
        checkboxInputGroupStyle
    ".radio" & do
        radioInputGroupStyle