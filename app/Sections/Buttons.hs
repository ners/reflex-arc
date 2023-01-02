module Sections.Buttons where

import Arc.Util
import Arc.Widgets.Button
import Arc.Widgets.Icon
import Control.Monad (forM_)
import Reflex.Dom hiding (button)

buttonsSection :: forall t m. DomBuilder t m => m ()
buttonsSection = do
    el "h2" $ text "Buttons"
    el "h3" $ text "Button variants"
    buttons $ \variant ->
        button $ def{buttonVariant = variant, buttonContent = tshow variant}
    el "h3" $ text "Disabled buttons"
    buttons $ \variant ->
        button $ def{buttonVariant = variant, buttonContent = tshow variant, buttonDisabled = True}
    el "h3" $ text "Buttons with icons"
    forM_ [minBound .. maxBound] $ \variant -> do
        let star = Just $ mdiIcon mdiStar
        elClass "div" "buttons"
            $ forM_
                [ (content, leftIcon, rightIcon)
                | content <- ["Button", ""]
                , leftIcon <- [Nothing, star]
                , rightIcon <- [Nothing, star]
                ]
            $ \(content, leftIcon, rightIcon) -> do
                button
                    def
                        { buttonVariant = variant
                        , buttonContent = content
                        , buttonLeftIcon = leftIcon
                        , buttonRightIcon = rightIcon
                        }
  where
    buttons :: (ButtonVariant -> m a) -> m ()
    buttons = elClass "div" "buttons" . forM_ [minBound .. maxBound]
