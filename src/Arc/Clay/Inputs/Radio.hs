module Arc.Clay.Inputs.Radio where

import Arc.Clay.Colours
import Arc.Clay.Util
import Clay

radioInputStyle :: Css
radioInputStyle = do
    width $ em 0.8
    height $ em 0.8
    borderRadiusAll $ pct 50
    backgroundColor radioBackground
    border (em 0.1) solid radioBorderUnchecked
    transition "border" (ms 100) easeInOut (sec 0)
    transition "background" (ms 100) easeInOut (sec 0)
    position absolute
    top $ em 0.35
    left nil
    marginAll nil
    --marginRight $ em 0.5
    checked & do
        border (em 0.3) solid radioBorderChecked
    disabled & do
        backgroundColor radioBorderUnchecked
        sibling label ? do
            cursor notAllowed
            opacity 0.5
    (self <> sibling label) ? do
        transition "opacity" (ms 100) easeInOut (sec 0)
        cursor pointer
  where
    radioBorderUnchecked = rgba 223 225 230 1
    radioBackground = rgba 250 251 252 1
    radioBorderChecked = primaryBlue

radioInputGroupStyle :: Css
radioInputGroupStyle = do
    paddingLeft $ em 1.5
