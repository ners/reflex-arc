module Arc.Clay.Message where

import Arc.Clay.Util
import Arc.Util
import Arc.Widgets.Message (Message (Message), MessageVariant (..))
import Clay
import Clay.Stylesheet (key)

messages :: Css
messages = baseClass_ @Message ? messageStyle

messageStyle :: Css
messageStyle = do
    fontSize (pct 75)
    class_ WarningMessage & do
        lineHeight (em 2)
        color (rgba 255 175 0 1)
