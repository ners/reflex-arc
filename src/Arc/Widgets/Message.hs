module Arc.Widgets.Message where

import Arc.Tokens.Size (SizeToken (SmallSize))
import Arc.Util
import Arc.Widgets.Icon
import Data.Default (Default)
import Data.Text (Text)
import Reflex.Dom

data MessageVariant = InformationMessage | ErrorMessage | SuccessMessage | WarningMessage
    deriving stock (Show)

instance ClassName MessageVariant

data Message = Message
    { messageVariant :: MessageVariant
    , messageContent :: Text
    }

instance ClassName Message where
    className = className . messageVariant

instance BaseClassName Message where
    baseClassName = "message"

instance Default Message where
    def = Message InformationMessage ""

message :: DomBuilder t m => Message -> m ()
message m@Message{..} =
    iconWithTextClass
        ("icon-with-text " <> fullClassString m)
        (def{iconSize = SmallSize}) -- , iconImage = mdiAlertRhombus}) TODO
        messageContent
