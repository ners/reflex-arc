module Arc.Clay.Layouts where

import Arc.Clay.Layouts.Grid
import Arc.Clay.Layouts.ListDetail
import Arc.Clay.Layouts.Tabs
import Clay (Css)

layouts :: Css
layouts = do
    listDetail
    tabs
    grid
