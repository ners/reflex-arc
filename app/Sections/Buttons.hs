module Sections.Buttons where

import Arc.Util
import Arc.Widgets.Button
import Arc.Widgets.Icon
import Control.Monad (forM_)
import Reflex.Dom hiding (button)

buttonsSection :: DomBuilder t m => m ()
buttonsSection = do
    el "h2" $ text "Buttons"
    el "h3" $ text "Button variants"
    forM_ [minBound .. maxBound] $ \variant ->
        button $ def{buttonVariant = variant, buttonContent = tshow variant}
    el "h3" $ text "Disabled buttons"
    forM_ [minBound .. maxBound] $ \variant ->
        button $ def{buttonVariant = variant, buttonContent = tshow variant, buttonDisabled = True}
    el "h3" $ text "Buttons with icons"
    forM_ [minBound .. maxBound] $ \variant -> el "div" $ do
        let star = Just $ mdiIcon mdiStar
        forM_
            [ (content, leftIcon, rightIcon)
            | content <- ["Button", ""]
            , leftIcon <- [Nothing, star]
            , rightIcon <- [Nothing, star]
            ]
            $ \(content, leftIcon, rightIcon) -> do
                button $
                    def
                        { buttonVariant = variant
                        , buttonContent = content
                        , buttonLeftIcon = leftIcon
                        , buttonRightIcon = rightIcon
                        }
