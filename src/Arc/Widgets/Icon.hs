{-# LANGUAGE RankNTypes #-}

module Arc.Widgets.Icon where

import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Svg
import Data.Default (Default)
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex.Dom

data IconSize = Small | Medium | Large

instance Show IconSize where
    show Small = "small"
    show Medium = "medium"
    show Large = "large"

instance IsSize IconSize where
    size Small = Em 1
    size Medium = Em 1.5
    size Large = Em 2

data Icon = Icon
    { iconSize :: IconSize
    , iconImage :: forall t m. (DomBuilder t m, PostBuild t m) => m ()
    }

instance Default Icon where
    def = Icon Small blank

icon :: (DomBuilder t m, PostBuild t m) => Icon -> m ()
icon Icon{..} = span iconImage
  where
    span = elDynAttr "span" $ mkDynAttrs [Just ("role", "img"), Just ("class", "icon " <> tshow iconSize)]
