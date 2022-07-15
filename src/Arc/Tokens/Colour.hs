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
    foregroundColour DangerButton = white
    foregroundColour DefaultButton = white
    foregroundColour GhostButton = white
    foregroundColour PrimaryButton = white
    foregroundColour WarningButton = white

instance ColourToken MessageVariant where
    foregroundColour ErrorMessage = white
    foregroundColour InformationMessage = white
    foregroundColour SuccessMessage = white
    foregroundColour WarningMessage = white
