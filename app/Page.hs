{-# LANGUAGE NoImplicitPrelude #-}

module Page where

import Arc.Layouts.Page
import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Button
import Arc.Widgets.Form
import Arc.Widgets.Icon
import Control.Monad (forM_, void)
import Debug.Trace (traceM)
import Reflex.Dom hiding (button)
import Reflex.Dom.MDI
import Signup
import Prelude hiding (div)

data MainPage

instance PageLayout MainPage where
    pageHeader = Just $
        forM_ [(tag, size) | tag <- ["h1"], size <- reverse [LargeSize]] $ \(tag, size) -> do
            el tag $ do
                iconWithText (def{iconImage = mdiChartArc, iconSize = size}) "Reflex Arc"
                el "span" $ text "Design System"
    pageMain = void $ form @SignupForm

page :: forall w. Widget w ()
page = pageLayout @MainPage
