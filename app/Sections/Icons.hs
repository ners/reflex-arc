{-# LANGUAGE LambdaCase #-}

module Sections.Icons where

import Arc.Layouts.Grid
import Arc.Layouts.Tabs
import Arc.Tokens.ArcLogo (arcLogoIcon)
import Arc.Util
import Arc.Widgets.Icon
import Arc.Widgets.Nav
import Control.Monad (forM_, void)
import Control.Monad.Fix (MonadFix)
import Data.Text qualified as Text
import Reflex.Dom hiding (tag)

data IconSection = IconExplorer | IconExamples
    deriving stock (Eq, Bounded, Enum)

instance Show IconSection where
    show IconExplorer = "Icon explorer"
    show IconExamples = "Examples"

instance Clickable IconSection

instance Selectable IconSection

instance Nav IconSection

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
    gridContent mdi = never <$ icon (mdiIcon mdi)

iconExplorer :: DomBuilder t m => m ()
iconExplorer = () <$ grid @MDI @()

iconExamples :: DomBuilder t m => m ()
iconExamples = do
    forM_ [(tag, size) | tag <- ["h1", "h2", "h3", "div"], size <- [minBound .. maxBound]] $ \(tag, size) -> do
        el "hr" blank
        el "div" $ text $ Text.unwords [tag, tshow size]
        forM_
            [ (i, content)
            | i <- [mdiIcon MdiAccountCircle, arcLogoIcon]
            , content <- [Nothing, Just "with text"]
            ]
            $ \(i, content) -> do
                el tag $ do
                    let icn = i{iconSize = size}
                    case content of
                        Just t -> iconWithText icn t
                        Nothing -> icon icn
                    el "span" $ text "surrounding text"
