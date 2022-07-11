{-# LANGUAGE RankNTypes #-}

module Arc.Main where

import Arc.Clay.App
import Arc.Clay.Util
import Reflex.Dom

arcMain :: (forall w. Widget w ()) -> IO ()
arcMain = mainWidgetWithCss (renderBS appStyle)
