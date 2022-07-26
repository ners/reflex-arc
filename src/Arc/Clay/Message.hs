module Arc.Clay.Message where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Arc.Util
import Arc.Widgets.Message (Message (Message), MessageVariant (..))
import Clay
import Clay.Stylesheet (key)

instance ColourToken MessageVariant where
    foregroundColour ErrorMessage = textWhite
    foregroundColour InformationMessage = textDefault
    foregroundColour SuccessMessage = textWhite
    foregroundColour WarningMessage = textDefaultDark
    backgroundColour ErrorMessage = errorColour
    backgroundColour InformationMessage = primaryColour
    backgroundColour SuccessMessage = successColour
    backgroundColour WarningMessage = warningColour

messages :: Css
messages = baseClass_ @Message ? messageStyle

messageStyle :: Css
messageStyle = do
    fontSize (pct 75)
    class_ WarningMessage & do
        lineHeight (em 2)
        color $ backgroundColour WarningMessage
