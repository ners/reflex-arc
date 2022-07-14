module Arc.Clay.Buttons where

import Arc.Clay.Colours
import Arc.Clay.Util
import Clay

buttons :: Css
buttons = button ? buttonStyle

buttonStyle :: Css
buttonStyle = do
    borderWidth $ em 0
    borderStyle none
    borderRadiusAll $ em 0.1875
    display inlineBlock
    height $ em 2
    lineHeight $ em 2
    marginAll $ em 0.2
    padding2 (em 0) (em 1)
    fontWeight $ weight 500
    cursor pointer
    position relative
    self |> (star <> ".icon") ? do
        verticalAlign middle
        margin2 (em 0) (em 0.3)
    ".DefaultButton" & do
        color $ rgb 66 82 110
        backgroundColor $ rgba 9 30 66 0.04
        hover & backgroundColor (rgba 9 30 66 0.08)
        active & backgroundColor (rgba 179 212 255 0.6)
    ".PrimaryButton" & do
        color primaryWhite
        backgroundColor primaryBlue
        hover & backgroundColor (rgb 0 101 255)
        active & backgroundColor (rgb 7 71 166)
    ".WarningButton" & do
        color $ rgb 23 43 77
        backgroundColor $ rgb 255 171 0
        hover & backgroundColor (rgb 255 196 0)
        active & backgroundColor (rgb 255 153 31)
    ".DangerButton" & do
        color primaryWhite
        backgroundColor $ rgb 222 53 11
        hover & backgroundColor (rgb 255 86 48)
        active & backgroundColor (rgb 191 38 0)
    disabled & do
        opacity 0.5
        pointerEvents none
