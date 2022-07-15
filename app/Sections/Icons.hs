module Sections.Icons where

import Arc.Tokens.Size
import Arc.Widgets.Icon
import Arc.Widgets.Message
import Reflex.Dom
import Reflex.Dom.MDI (mdiStar)

iconsSection :: DomBuilder t m => m ()
iconsSection = do
    message $ def{messageContent = "Validation error", messageVariant = WarningMessage}
