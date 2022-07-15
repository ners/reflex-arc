module Arc.Clay.Layouts.ListDetail where

import Clay

listDetail :: Css
listDetail = ".list-detail" ? listDetailStyle

listDetailStyle :: Css
listDetailStyle = do
    flexGrow 1
    display flex
    flexDirection row
    -- ".list" ?
    ".detail" ? do
        flexGrow 1
