{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Widgets.Icon
    ( module Web.Font.MDI
    , Icon (..)
    , iconWithText
    , iconWithTextClass
    ) where

import Arc.Tokens.Size
import Arc.Util
import Data.Text (Text)
import Reflex.Dom
import Web.Font.MDI

class Icon i where
    iconSize :: i -> SizeToken
    iconSize _ = MediumSize
    iconClass :: i -> Text
    iconClass i = "icon " <> className (iconSize i)
    iconContent :: DomBuilder t m => i -> m ()
    default iconContent :: (ToElement i, DomBuilder t m) => i -> m ()
    iconContent = toElement
    icon :: DomBuilder t m => i -> m ()
    icon i = elAttr "span" attrs $ iconContent i
      where
        attrs = mkAttrs [Just ("role", "img"), Just ("class", iconClass i)]

iconWithText :: Icon i => DomBuilder t m => i -> Text -> m ()
iconWithText = iconWithTextClass "icon-with-text"

iconWithTextClass :: Icon i => DomBuilder t m => Text -> i -> Text -> m ()
iconWithTextClass c i t = elClass "span" c $ do
    icon i
    elClass "span" "text" $ text t

instance Icon i => Icon (Maybe i) where
    iconContent = maybe blank iconContent

instance Icon i => Icon (SizeToken, i) where
    iconSize = fst
    iconContent = iconContent . snd

instance Icon () where
    icon () = blank

instance ToElement MDI where
    toElement = toElement . mdiChar

instance Icon MDI
