module Arc.Tokens.Colour where

import Arc.Clay.Util
import Arc.Widgets.Button (ButtonVariant (..))
import Arc.Widgets.Message (MessageVariant (..))
import Clay

class ColourToken ct where
    foregroundColour :: ct -> Color
    backgroundColour :: ct -> Color
    backgroundColour = const transparent

instance ColourToken ButtonVariant where
    foregroundColour PrimaryButton = textWhite
    foregroundColour WarningButton = textDefaultDark
    foregroundColour DefaultButton = textDefault
    foregroundColour DangerButton = textWhite
    foregroundColour GhostButton = textDefault
    backgroundColour PrimaryButton = primaryColour
    backgroundColour WarningButton = warningColour
    backgroundColour DefaultButton = defaultColour
    backgroundColour DangerButton = errorColour
    backgroundColour GhostButton = ghostColour

instance ColourToken MessageVariant where
    foregroundColour ErrorMessage = textWhite
    foregroundColour InformationMessage = textDefault
    foregroundColour SuccessMessage = textWhite
    foregroundColour WarningMessage = textDefaultDark
    backgroundColour ErrorMessage = errorColour
    backgroundColour InformationMessage = primaryColour
    backgroundColour SuccessMessage = successColour
    backgroundColour WarningMessage = warningColour

-- Colour values

errorRed :: Color
errorRed = rgba 222 53 11 1

primaryBlue :: Color
primaryBlue = rgba 0 82 204 1

primaryGrey :: Color
primaryGrey = rgba 9 30 66 0.04

primaryWhite :: Color
primaryWhite = rgba 255 255 255 1

successGreen :: Color
successGreen = rgba 0 102 68 1

warningOrange :: Color
warningOrange = rgba 255 171 0 1

defaultBlack :: Color
defaultBlack = rgba 66 82 110 1

textDefault :: Color
textDefault = defaultBlack

textWhite :: Color
textWhite = blankColour

textDefaultDark :: Color
textDefaultDark = rgba 23 43 77 1

-- Button background colours

blankColour :: Color
blankColour = primaryWhite

defaultColour :: Color
defaultColour = primaryGrey

errorColour :: Color
errorColour = errorRed

ghostColour :: Color
ghostColour = primaryGrey

primaryColour :: Color
primaryColour = primaryBlue

successColour :: Color
successColour = successGreen

warningColour :: Color
warningColour = warningOrange
