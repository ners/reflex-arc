{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Layouts.Page where

import Control.Monad.Fix (MonadFix)
import Reflex.Dom

class PageLayout p e where
    pageHeader :: DomBuilder t m => Maybe (m (Event t e))
    pageHeader = Nothing
    pageMain
        :: DomBuilder t m
        => PostBuild t m
        => MonadHold t m
        => MonadFix m
        => m (Event t e)
    pageFooter :: DomBuilder t m => Maybe (m (Event t e))
    pageFooter = Nothing

pageLayout
    :: forall p e t m
     . PageLayout p e
    => DomBuilder t m
    => PostBuild t m
    => MonadHold t m
    => MonadFix m
    => m (Event t e)
pageLayout = do
    maybe (pure never) (el "header") $ pageHeader @p @e
    el "main" $ pageMain @p @e
    maybe (pure never) (el "footer") $ pageFooter @p @e
