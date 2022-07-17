module Arc.Clay.Buttons where

import Arc.Tokens.Colour
import Arc.Clay.Util
import Clay
import Arc.Widgets.Button (ButtonVariant(..))
import Arc.Widgets.Icon

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
    self |> (star <> baseClass_ @Icon) ? do
        margin2 (em 0) (em 0.3)
    class_ GhostButton & do
        color textDefault
        backgroundColor transparent
        fontWeight normal
        hover & backgroundColor (rgba 9 30 66 0.04)
        active & backgroundColor (rgba 9 30 66 0.08)
    class_ DefaultButton & do
        color textDefault
        backgroundColor defaultColour
        hover & backgroundColor (rgba 9 30 66 0.08)
        active & backgroundColor (rgba 179 212 255 0.6)
    class_ PrimaryButton & do
        color textWhite
        backgroundColor primaryColour
        hover & backgroundColor (rgb 0 101 255)
        active & backgroundColor (rgb 7 71 166)
    class_ WarningButton & do
        color textDefaultDark
        backgroundColor warningColour
        hover & backgroundColor (rgb 255 196 0)
        active & backgroundColor (rgb 255 153 31)
    class_ DangerButton & do
        color textWhite
        backgroundColor errorColour
        hover & backgroundColor (rgb 255 86 48)
        active & backgroundColor (rgb 191 38 0)
    disabled & do
        opacity 0.5
        pointerEvents none
