{-# LANGUAGE LambdaCase #-}

module Page where

import Arc.Layouts.ListDetail
import Arc.Layouts.Page
import Arc.Tokens.ArcLogo
import Arc.Tokens.Size
import Arc.Util
import Arc.Widgets.Icon
import Arc.Widgets.Nav
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Reflex.Dom
import Sections.About (aboutSection)
import Sections.Buttons (buttonsSection)
import Sections.Code (codeSection)
import Sections.Forms (formsSection)
import Sections.Icons (iconsSection)
import Sections.Text (textSection)
import Prelude hiding (div)

data MainPage

data PageSection = About | Buttons | Forms | Icons | Text | Code
    deriving stock (Eq, Ord, Bounded, Enum, Show)

instance Clickable PageSection
instance Selectable PageSection
instance Nav PageSection

instance ListDetail PageSection where
    listInitial = Just About
    listView d = Just <<$>> nav @PageSection d
    detailView d = void $
        update d $ \case
            Just About -> aboutSection
            Just Buttons -> buttonsSection
            Just Forms -> formsSection
            Just Icons -> iconsSection
            Just Text -> textSection
            Just Code -> codeSection
            _ -> blank

data ThemeToggle = ThemeToggle

instance Clickable ThemeToggle where
    clickableClass = "theme"
    clickableContent ThemeToggle = icon $ mdiIcon MdiBrightness6

instance PageLayout MainPage ThemeToggle where
    pageHeader = Just $
        el "h1" $ do
            iconWithText (arcLogoIcon{iconSize = MediumSize}) "Reflex Arc"
            el "span" $ text "Design System"
            clickable ThemeToggle >>= (\e -> pure $ ThemeToggle <$ e)
    pageMain = do
        listDetail @PageSection
        pure never

page
    :: DomBuilder t m
    => PostBuild t m
    => MonadHold t m
    => MonadFix m
    => m ()
page = void $ pageLayout @MainPage @ThemeToggle
