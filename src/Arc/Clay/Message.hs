module Arc.Clay.Message where

import Arc.Clay.Util
import Arc.Tokens.Colour
import Arc.Widgets.Message
import Clay hiding (black, blue, green, grey, orange, red, white)

instance ColourToken MessageVariant where
    foregroundColour ErrorMessage = white
    foregroundColour InformationMessage = black
    foregroundColour SuccessMessage = white
    foregroundColour WarningMessage = grey
    backgroundColour ErrorMessage = red
    backgroundColour InformationMessage = blue
    backgroundColour SuccessMessage = green
    backgroundColour WarningMessage = orange

messages :: Css
messages = baseClass_ @Message ? messageStyle

messageStyle :: Css
messageStyle = do
    fontSize (pct 75)
    class_ WarningMessage & do
        lineHeight (em 2)
        color $ backgroundColour WarningMessage
