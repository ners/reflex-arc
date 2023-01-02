{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Layouts.Page where

import Control.Monad.Fix (MonadFix)
import Reflex.Dom

class PageLayout p a where
    pageHeader :: DomBuilder t m => Maybe (m (Event t a))
    pageHeader = Nothing
    pageMain
        :: DomBuilder t m
        => PostBuild t m
        => MonadHold t m
        => MonadFix m
        => m (Event t a)
    pageFooter :: DomBuilder t m => Maybe (m (Event t a))
    pageFooter = Nothing

pageLayout
    :: forall p a t m
     . PageLayout p a
    => DomBuilder t m
    => PostBuild t m
    => MonadHold t m
    => MonadFix m
    => m (Event t a)
pageLayout = do
    maybe (pure never) (el "header") $ pageHeader @p @a
    el "main" $ pageMain @p @a
    maybe (pure never) (el "footer") $ pageFooter @p @a
