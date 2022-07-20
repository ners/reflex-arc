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
ghostColour = primaryGrey

primaryColour :: Color
primaryColour = primaryBlue

successColour :: Color
successColour = successGreen

warningColour :: Color
warningColour = warningOrange
