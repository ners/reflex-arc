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
import Data.Functor ((<&>))
import Reflex.Dom
import Sections.About (aboutSection)
import Sections.Buttons (buttonsSection)
import Sections.Code (codeSection)
import Sections.Forms (formsSection)
import Sections.Icons (iconsSection)
import Sections.Text (textSection)

data MainPage

data PageSection = About | Buttons | Forms | Icons | Text | Code
    deriving stock (Show, Eq, Ord, Bounded, Enum)
    deriving anyclass (ToElement, Clickable, Selectable, Nav)

instance ListDetail PageSection where
    listInitial = Just About
    listView d = Just <$$> nav @PageSection d
    detailClassName d = tshow <$$> d
    detailView d = void $ update d $ maybe blank $ \case
        About -> aboutSection
        Buttons -> buttonsSection
        Forms -> formsSection
        Icons -> iconsSection
        Text -> textSection
        Code -> codeSection

data ThemeToggle = ThemeToggle

instance Icon ThemeToggle where
    iconSize ThemeToggle = SmallSize
    iconContent ThemeToggle = toElement MdiBrightness6

instance Clickable ThemeToggle where
    clickableClass = "theme"
    clickableContent = icon @ThemeToggle

instance PageLayout MainPage ThemeToggle where
    pageHeader = Just $
        el "h1" $ do
            iconWithText ArcLogo "Reflex Arc"
            text " "
            el "span" $ text "Design System"
            clickable ThemeToggle <&> (ThemeToggle <$)
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
