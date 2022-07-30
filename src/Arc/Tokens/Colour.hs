module Arc.Tokens.Colour where

import Arc.Clay.Util
import Clay

class ColourToken ct where
    foregroundColour :: ct -> Color
    backgroundColour :: ct -> Color
    backgroundColour = const transparent

-- Colour values

errorRed :: Color
errorRed = rgb 222 53 11

primaryBlue :: Color
primaryBlue = rgb 0 82 204

primaryGrey :: Color
primaryGrey = rgb 9 30 66

primaryWhite :: Color
primaryWhite = rgb 255 255 255

successGreen :: Color
successGreen = rgb 0 102 68

warningOrange :: Color
warningOrange = rgb 255 171 0

defaultBlack :: Color
defaultBlack = rgb 66 82 110

textDefault :: Color
textDefault = defaultBlack

textWhite :: Color
textWhite = blankColour

textDefaultDark :: Color
textDefaultDark = rgb 23 43 77

-- Button background colours

blankColour :: Color
blankColour = primaryWhite

defaultColour :: Color
defaultColour = primaryGrey

errorColour :: Color
errorColour = errorRed

ghostColour :: Color
ghostColour = transparent

primaryColour :: Color
primaryColour = primaryBlue

successColour :: Color
successColour = successGreen

warningColour :: Color
warningColour = warningOrange
