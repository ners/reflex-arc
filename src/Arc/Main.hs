module Arc.Main where

import Arc.Clay.App
import Arc.Clay.Util
import Clay (Css)
import Reflex.Dom

arcMain :: (forall w. Widget w ()) -> IO ()
arcMain = mainWidgetWithCss $ renderBS appStyle

arcMainWithCss :: Css -> (forall w. Widget w ()) -> IO ()
arcMainWithCss css = mainWidgetWithCss $ renderBS $ appStyle >> css
