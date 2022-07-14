{-# LANGUAGE NoImplicitPrelude #-}

module Page where

import Arc.Layouts.Page
import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Button
import Arc.Widgets.Form
import Arc.Widgets.Icon
import Control.Monad (forM_)
import Reflex.Dom hiding (button)
import Reflex.Dom.MDI
import Signup
import Prelude hiding (div)

data MainPage

instance PageLayout MainPage where
    pageHeader = Just $
        el "h1" $ do
            icon $ def{iconImage = mdiChartArc, iconSize = LargeSize}
            el "span" $ text "Reflex Arc Design System"
    pageMain = do
        form @SignupForm
        forM_
            [ (icon1, icon2, disabled)
            | icon1 <- [Nothing, Just $ def{iconImage = mdiStar}]
            , icon2 <- [Nothing, Just $ def{iconImage = mdiStar}]
            , disabled <- [False, True]
            ]
            $ \(icon1, icon2, disabled) -> div $
                forM_ [DangerButton, WarningButton, PrimaryButton, DefaultButton] $ \variant ->
                    button $
                        Button
                            { buttonContent = "Button"
                            , buttonVariant = variant
                            , buttonLeftIcon = icon1
                            , buttonRightIcon = icon2
                            , buttonDisabled = disabled
                            }

page :: forall w. Widget w ()
page = pageLayout @MainPage
