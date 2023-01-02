module Arc.Widgets.Button where

import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Icon
import Data.Default
import Data.Text (Text)
import Reflex.Dom

data ButtonVariant = GhostButton | DefaultButton | PrimaryButton | WarningButton | DangerButton
    deriving stock (Show, Bounded, Enum)

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

button :: forall t m. DomBuilder t m => Button -> m (Event t ())
button Button{..} = buttonEl >>= \(e, _) -> return $ domEvent Click e
  where
    attrs = mkAttrs [Just ("class", className buttonVariant), maybeDisabled buttonDisabled]
    buttonEl = elAttr' "button" attrs $ do
        mapM_ iconEl buttonLeftIcon
        el "span" $ text buttonContent
        mapM_ iconEl buttonRightIcon
    iconEl :: Icon -> m ()
    iconEl i = icon $ i{iconSize = SmallSize}
