{-# LANGUAGE NoImplicitPrelude #-}

module Page where

import Arc.Layouts.ListDetail
import Arc.Layouts.Page
import Arc.Tokens.ArcLogo
import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Button
import Arc.Widgets.Form
import Arc.Widgets.Icon
import Arc.Widgets.Nav
import Arc.Widgets.Svg
import Control.Monad (forM_, void)
import Debug.Trace (traceM)
import Reflex.Dom hiding (button)
import Sections.Buttons
import Sections.Code
import Sections.Forms
import Sections.Icons
import Sections.Text
import Prelude hiding (div)

data MainPage

data PageSection = About | Buttons | Forms | Icons | Text | Code
    deriving (Eq, Ord, Bounded, Enum, Show)

instance Clickable PageSection
instance Selectable PageSection
instance Nav PageSection

instance ListDetail PageSection where
    listInitial = Just About
    listView d = Just <<$>> nav @PageSection d
    detailView d = void $
        update d $ \case
            Just About -> el "div" $ text "Introduction..."
            Just Buttons -> buttonsSection
            Just Forms -> formsSection
            Just Icons -> iconsSection
            Just Text -> textSection
            Just Code -> codeSection
            _ -> blank

instance PageLayout MainPage where
    pageHeader = Just $
        el "h1" $ do
            iconWithText (arcLogoIcon{iconSize = MediumSize}) "Reflex Arc"
            el "span" $ text "Design System"
    pageMain = listDetail @PageSection

page :: Widget w ()
page = pageLayout @MainPage
