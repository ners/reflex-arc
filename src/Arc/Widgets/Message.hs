module Arc.Widgets.Message where

import Arc.Tokens.Size (SizeToken (SmallSize))
import Arc.Util
import Arc.Widgets.Icon
import Data.Default (Default)
import Data.Text
import Reflex.Dom
import Reflex.Dom.MDI (mdiAlertRhombus)

data MessageVariant = InformationMessage | ErrorMessage | SuccessMessage | WarningMessage
    deriving (Show)

instance ClassName MessageVariant

data Message = Message
    { messageVariant :: MessageVariant
    , messageContent :: Text
    }

instance Default Message where
    def = Message InformationMessage ""

message :: DomBuilder t m => Message -> m ()
message Message{..} =
    iconWithTextClass
        ("icon-with-text message " <> className messageVariant)
        (def{iconSize = SmallSize, iconImage = mdiAlertRhombus})
        messageContent
