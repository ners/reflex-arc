module Arc.Widgets.Button where

import Arc.Tokens.Colour
import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Icon
import Clay (transparent)
import Control.Monad (forM_, when)
import Data.Default
import Data.Maybe (fromJust, isJust)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex.Dom
import qualified Reflex.Dom as ReflexDom

instance ColourToken ButtonVariant where
    foregroundColour PrimaryButton = white
    foregroundColour WarningButton = grey
    foregroundColour DefaultButton = black
    foregroundColour DangerButton = white
    foregroundColour GhostButton = black
    backgroundColour PrimaryButton = blue
    backgroundColour WarningButton = orange
    backgroundColour DefaultButton = grey
    backgroundColour DangerButton = red
    backgroundColour GhostButton = transparent

data ButtonVariant = GhostButton | DefaultButton | PrimaryButton | WarningButton | DangerButton
    deriving (Show, Bounded, Enum)

data Button = Button
    { buttonContent :: Text
    , buttonVariant :: ButtonVariant
    , buttonLeftIcon :: Maybe Icon
    , buttonRightIcon :: Maybe Icon
    , buttonDisabled :: Bool
    }

instance Default Button where
    def = Button "" DefaultButton Nothing Nothing False

instance ClassName ButtonVariant

instance ClassName Button where
    className = className . buttonVariant

button :: DomBuilder t m => Button -> m (Event t ())
button Button{..} = buttonEl >>= \(e, _) -> return $ domEvent Click e
  where
    attrs = mkAttrs [Just ("class", className buttonVariant), maybeDisabled buttonDisabled]
    buttonEl = elAttr' "button" attrs $ do
        mapM_ iconEl buttonLeftIcon
        el "span" $ text buttonContent
        mapM_ iconEl buttonRightIcon
    iconEl i = icon $ i{iconSize = SmallSize}
