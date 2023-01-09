module Sections.Buttons where

import Arc.Util
import Arc.Widgets.Button
import Arc.Widgets.Icon
import Control.Monad (void)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Reflex.Dom hiding (button)

instance Button ButtonVariant () where
    buttonText bv = fromMaybe (tshow bv) $ Text.stripSuffix "Button" $ tshow bv
    buttonVariant = id
    buttonDisabled _ = pure False

instance ButtonGroup ButtonVariant ()

newtype DisabledButtonVariant = DBV {bv :: ButtonVariant}
    deriving newtype (Bounded, Enum)

instance Button DisabledButtonVariant () where
    buttonText DBV{..} = fromMaybe (tshow bv) $ Text.stripSuffix "Button" $ tshow bv
    buttonVariant = bv
    buttonDisabled _ = pure True

instance ButtonGroup DisabledButtonVariant ()

buttonsSection :: forall t m. (DomBuilder t m, PostBuild t m) => m ()
buttonsSection = do
    el "h2" $ text "Buttons"
    el "h3" $ text "Variants"
    void $ buttonGroup @ButtonVariant @()
    el "h3" $ text "Disabled"
    void $ buttonGroup @DisabledButtonVariant @()
    el "h3" $ text "Icons"
    elClass "div" "buttons" $
        mapM_
            (button @(Maybe MDI, Text, Maybe MDI) @MDI)
            [ (leftIcon, content, rightIcon)
            | content <- ["", "Button"]
            , rightIcon <- [Nothing, Just MdiChevronRightCircle]
            , leftIcon <- [Nothing, Just MdiChevronLeftCircle]
            , isJust leftIcon || isJust rightIcon
            ]
