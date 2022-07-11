module Arc.Widgets.Button where

import Arc.Util
import Arc.Widgets.Icon (Icon (Icon, iconSize), IconSize (Small), icon)
import Control.Monad (forM_, when)
import Data.Default
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex.Dom
import qualified Reflex.Dom as ReflexDom

data ButtonVariant = Default | Primary | Warning | Danger
    deriving (Show)

data Button = Button
    { buttonContent :: Text
    , buttonVariant :: ButtonVariant
    , buttonLeftIcon :: Maybe Icon
    , buttonRightIcon :: Maybe Icon
    , buttonDisabled :: Bool
    }

instance Default Button where
    def = Button "" Default Nothing Nothing False

button :: (DomBuilder t m, PostBuild t m) => Button -> m (Event t ())
button Button{..} = buttonEl >>= \(e, _) -> return $ domEvent Click e
  where
    attrs = mkDynAttrs $ ("class", Text.unwords [tshow buttonVariant]) : [("disabled", "disabled") | buttonDisabled]
    buttonEl = elDynAttr' "button" attrs $ do
        mapM_ iconEl buttonLeftIcon
        el "span" $ text buttonContent
        mapM_ iconEl buttonRightIcon
    iconEl i = icon $ i{iconSize = Small}
