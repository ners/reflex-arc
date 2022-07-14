{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Layouts.Page where

import Data.Default (Default)
import Reflex.Dom

class PageLayout p where
    pageHeader :: forall t m. DomBuilder t m => Maybe (m ())
    pageHeader = Nothing
    pageMain :: forall t m. DomBuilder t m => m ()
    pageFooter :: forall t m. DomBuilder t m => Maybe (m ())
    pageFooter = Nothing

pageLayout :: forall p t m. (PageLayout p, DomBuilder t m) => m ()
pageLayout = do
    maybe blank (el "header") $ pageHeader @p
    el "main" $ pageMain @p
    maybe blank (el "footer") $ pageFooter @p
