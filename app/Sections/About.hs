{-# LANGUAGE LambdaCase #-}

module Sections.About where

import Arc.Layouts.Tabs
import Arc.Util
import Arc.Widgets.Nav
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Reflex.Dom

data About = Introduction | Test1 | Test2
    deriving stock (Show, Eq, Bounded, Enum)

instance Clickable About

instance Selectable About

instance Nav About

instance Tabs About where
    initialTab = Just Introduction
    tabView d = void $ update d $ \case
        Just t -> text $ tshow t
        Nothing -> pure ()

aboutSection
    :: forall t m
     . DomBuilder t m
    => PostBuild t m
    => MonadHold t m
    => MonadFix m
    => m ()
aboutSection = tabs @About
