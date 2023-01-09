{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Arc.Widgets.Button where

import Arc.Util
import Arc.Widgets.Icon
import Control.Monad (forM, unless)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Reflex.Dom hiding (button)

data ButtonVariant = GhostButton | DefaultButton | PrimaryButton | WarningButton | DangerButton
    deriving stock (Show, Bounded, Enum)
    deriving anyclass (ClassName)

class Icon i => Button b i where
    buttonText :: b -> Text
    default buttonText :: Show b => b -> Text
    buttonText = tshow
    buttonVariant :: b -> ButtonVariant
    buttonVariant _ = DefaultButton
    buttonClass :: b -> Text
    buttonClass b = (if hasText && hasIcon then "icon-with-text " else "") <> tshow (buttonVariant @b @i b)
      where
        hasText = nonEmpty $ buttonText @b @i b
        hasIcon = any isJust [buttonLeftIcon @b @i b, buttonRightIcon b]
    buttonDisabled :: forall t m. DomBuilder t m => b -> Dynamic t Bool
    buttonDisabled _ = pure False
    buttonLeftIcon :: b -> Maybe i
    buttonLeftIcon _ = Nothing
    buttonRightIcon :: b -> Maybe i
    buttonRightIcon _ = Nothing
    button :: forall t m. (DomBuilder t m, PostBuild t m) => b -> m (Event t ())
    button b = buttonEl >>= \(e, _) -> return $ domEvent Click e
      where
        attrs = do
            d <- buttonDisabled @b @i @t @m b
            mkDynAttrs [Just ("class", buttonClass @b @i b), maybeDisabled d]
        t = buttonText @b @i b
        buttonEl = elDynAttr' "button" attrs $ do
            mapM_ (icon @i) $ buttonLeftIcon b
            unless (Text.null t) $ elClass "span" "text" $ text t
            mapM_ (icon @i) $ buttonRightIcon b

instance Icon i => Button () i where
    buttonText () = ""
    buttonDisabled _ = pure False

instance Icon i => Button Text i where
    buttonText = id
    buttonDisabled _ = pure False

instance Button (Maybe Text) () where
    buttonText = fromMaybe ""
    buttonDisabled _ = pure False

instance Button b i => Button (ButtonVariant, b) i where
    buttonText = buttonText @b @i . snd
    buttonVariant = fst
    buttonLeftIcon = buttonLeftIcon . snd
    buttonRightIcon = buttonRightIcon . snd
    buttonDisabled _ = pure False

instance Button b i => Button (Maybe i, b, Maybe i) i where
    buttonText (_, b, _) = buttonText @b @i b
    buttonVariant (_, b, _) = buttonVariant @b @i b
    buttonLeftIcon (i, _, _) = i
    buttonRightIcon (_, _, i) = i
    buttonDisabled _ = pure False

class Button b i => ButtonGroup b i where
    buttons :: [b]
    default buttons :: Enum b => Bounded b => [b]
    buttons = [minBound .. maxBound]
    buttonGroup :: forall t m. (DomBuilder t m, PostBuild t m) => m (Event t b)
    buttonGroup = divClass "buttons" $ leftmost <$> forM (buttons @b @i) (\b -> button @b @i b <&> (b <$))
