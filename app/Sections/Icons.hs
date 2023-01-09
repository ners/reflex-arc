{-# LANGUAGE LambdaCase #-}

module Sections.Icons where

import Arc.Layouts.Grid
import Arc.Layouts.Tabs
import Arc.Tokens.Size (SizeToken)
import Arc.Util
import Arc.Widgets.Icon
import Arc.Widgets.Nav
import Control.Monad (forM_, void)
import Control.Monad.Fix (MonadFix)
import Reflex.Dom hiding (tag)

data IconSection = IconExplorer | IconExamples
    deriving stock (Eq, Bounded, Enum)
    deriving anyclass (Clickable, Selectable, Nav)

instance ToElement IconSection where
    toElement IconExplorer = text "Icon explorer"
    toElement IconExamples = text "Examples"

instance Tabs IconSection where
    initialTab = Just IconExplorer
    tabView d = void $ update d $ \case
        Just IconExplorer -> iconExplorer
        Just IconExamples -> iconExamples
        Nothing -> pure ()

iconsSection
    :: forall t m
     . DomBuilder t m
    => PostBuild t m
    => MonadHold t m
    => MonadFix m
    => m ()
iconsSection = do
    el "h2" $ text "Icons"
    tabs @IconSection

deriving stock instance Bounded MDI
deriving stock instance Enum MDI

instance Grid MDI () where
    gridItems = take 100 [minBound .. maxBound]
    gridContent mdi = never <$ icon mdi

iconExplorer :: DomBuilder t m => m ()
iconExplorer = () <$ grid @MDI @()

iconExamples :: DomBuilder t m => m ()
iconExamples = do
    forM_ ["h1", "h2", "h3", "div"] $ \tag -> do
        el "hr" blank
        el "div" $ text tag
        forM_
            [ (i, size)
            | i <- [MdiAccountCircle]
            , size <- [minBound @SizeToken .. maxBound]
            ]
            $ \(i, size) -> do
                el tag $ do
                    icon (size, i)
                    text $ tshow size
                    iconWithText (size, i) "with text"
