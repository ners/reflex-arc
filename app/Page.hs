{-# LANGUAGE NoImplicitPrelude #-}

module Page where

import Arc.Util
import Arc.Widgets.Button
import Arc.Widgets.Form
import Arc.Widgets.Icon
import Control.Monad (forM_)
import Reflex.Dom hiding (button)
import Reflex.Dom.MDI
import Signup
import Prelude hiding (div)

page :: forall w. Widget w ()
page = do
    div $ do
        el "span" $ text "Hello world!!"
        icon $ def{iconImage = mdiTrophy}
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
                        { buttonContent = tshow variant
                        , buttonVariant = variant
                        , buttonLeftIcon = icon1
                        , buttonRightIcon = icon2
                        , buttonDisabled = disabled
                        }
