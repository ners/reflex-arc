{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Widgets.Message where

import Arc.Util
import Arc.Widgets.Icon
import Reflex.Dom

data MessageVariant = InformationMessage | ErrorMessage | SuccessMessage | WarningMessage
    deriving stock (Show)
    deriving anyclass (ClassName)

instance ToElement MessageVariant where
    toElement InformationMessage = icon MdiInformation
    toElement ErrorMessage = icon MdiCloseCircle
    toElement SuccessMessage = icon MdiCheckCircle
    toElement WarningMessage = icon MdiAlertCircle

instance Icon MessageVariant

class Show e => Message e where
    messageVariant :: e -> MessageVariant
    messageVariant = const InformationMessage
    message :: forall i t m. (Icon i, DomBuilder t m) => e -> m ()
    message m =
        iconWithTextClass
            ("icon-with-text message" <> className (messageVariant m))
            (messageVariant m)
            (tshow m)
