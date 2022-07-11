module Arc.Clay.App where

import Arc.Clay.Icons
import Arc.Clay.Fonts
import Arc.Clay.Inputs
import Arc.Clay.Buttons
import Arc.Clay.Util
import Clay
import Clay.Stylesheet (key)

appStyle :: Css
appStyle = do
    star ? globalStyle
    body ? bodyStyle
    fonts
    icons
    inputs
    buttons

globalStyle :: Css
globalStyle =
    star ? do
        key "appearance" $ Value "none"
        key "font" $ Value "inherit"
        key "color" $ Value "inherit"
        key "cursor" $ Value "inherit"
        key "highlight" $ Value "none"
        key "outline" $ Value "none"
        boxSizing borderBox

bodyStyle :: Css
bodyStyle = do
    fontSize $ px 16
    fontWeight $ weight 400
    fontFamily ["Source Sans"] []
    lineHeight $ em 1.5
