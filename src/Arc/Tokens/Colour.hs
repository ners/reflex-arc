module Arc.Tokens.Colour where

import Arc.Clay.Util
import Clay

class ColourToken ct where
    foregroundColour :: ct -> Color
    backgroundColour :: ct -> Color
    backgroundColour = const transparent

-- Colour values

red :: Color
red = rgb 222 53 11

blue :: Color
blue = rgb 0 82 204

grey :: Color
grey = rgb 9 30 66

white :: Color
white = rgb 255 255 255

green :: Color
green = rgb 0 102 68

orange :: Color
orange = rgb 255 171 0

black :: Color
black = rgb 66 82 110
