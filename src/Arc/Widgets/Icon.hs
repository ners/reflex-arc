module Arc.Widgets.Icon where

import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Svg
import Data.Default (Default)
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex.Dom

data Icon = Icon
    { iconSize :: SizeToken
    , iconImage :: forall t m. DomBuilder t m => m ()
    }

instance Default Icon where
    def = Icon SmallSize blank

icon :: DomBuilder t m => Icon -> m ()
icon Icon{..} = span iconImage
  where
    span = elAttr "span" $ mkAttrs [Just ("role", "img"), Just ("class", "icon " <> tshow iconSize)]

iconWithText :: DomBuilder t m => Icon -> Text -> m ()
iconWithText i t = elClass "span" "icon-with-text" $ do
    icon i
    elClass "span" "text" $ text t

mdiIcon :: Char -> Icon
mdiIcon c = def{iconImage = text $ Text.singleton c}
