module Arc.Widgets.Icon where

import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Svg
import Data.Default (Default)
import Data.Text (Text)
import Reflex.Dom

data Icon = Icon
    { iconSize :: SizeToken
    , iconImage :: forall t m. DomBuilder t m => m ()
    }

instance ClassName Icon where
    className = className . iconSize

instance BaseClassName Icon where
    baseClassName = "icon"

instance Default Icon where
    def = Icon SmallSize blank

icon :: DomBuilder t m => Icon -> m ()
icon i@Icon{..} = span iconImage
  where
    span = elAttr "span" $ mkAttrs [Just ("role", "img"), Just ("class", fullClassString i)]

iconWithText :: DomBuilder t m => Icon -> Text -> m ()
iconWithText = iconWithTextClass "icon-with-text"

iconWithTextClass :: DomBuilder t m => Text -> Icon -> Text -> m ()
iconWithTextClass c i t = elClass "span" c $ do
    icon i
    elClass "span" "text" $ text t
