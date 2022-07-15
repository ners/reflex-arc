{-# LANGUAGE AllowAmbiguousTypes #-}

module Arc.Layouts.Page where

import Data.Default (Default)
import Reflex.Dom

class PageLayout p where
    pageHeader :: Maybe (Widget w ())
    pageHeader = Nothing
    pageMain :: Widget w ()
    pageFooter :: Maybe (Widget w ())
    pageFooter = Nothing

pageLayout :: forall p w. PageLayout p => Widget w ()
pageLayout = do
    maybe blank (el "header") $ pageHeader @p
    el "main" $ pageMain @p
    maybe blank (el "footer") $ pageFooter @p
