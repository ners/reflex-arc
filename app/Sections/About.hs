module Sections.About where

import Arc.Layouts.Tabs
import Arc.Util
import Arc.Widgets.Nav
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Reflex.Dom

data About = Introduction | Test1 | Test2
    deriving stock (Show, Eq, Bounded, Enum)
    deriving anyclass (ToElement, Clickable, Selectable, Nav)

instance Tabs About where
    initialTab = Just Introduction
    tabView d = void $ update d $ maybe blank $ text . tshow

aboutSection
    :: forall t m
     . DomBuilder t m
    => PostBuild t m
    => MonadHold t m
    => MonadFix m
    => m ()
aboutSection = tabs @About
